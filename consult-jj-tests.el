;; -*- lexical-binding: t -*-

(require 'ert)
(require 'consult-jj)
(require 'consult-jj-diff)
(require 'consult-jj-describe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-sh (&rest commands)
  "Run shell COMMAND and additional COMMANDS, asserting each succeeds.

Each command may be a string or a list of strings (which are concatenated).
Runs synchronously in `default-directory'; returns the output of the last
command as a string."
  (with-temp-buffer
    (dolist (cmd commands)
      (let ((cmd-str (if (stringp cmd) cmd (string-join cmd " "))))
        (erase-buffer)
        (should (zerop (call-process shell-file-name nil t nil
                                     shell-command-switch cmd-str)))))
    (string-trim (buffer-string))))

(defmacro with-test-repo (&rest body)
  "Run BODY in a fresh temporary jj repository.

During BODY, `test-repo-buffer' is bound to the temporary buffer in
which the repository is set up.  The temporary directory is deleted
after BODY succeeds.  If BODY signals an error, the directory is kept
and its path is printed for inspection."
  (declare (indent 0) (debug body))
  (let ((temp-dir  (gensym "temp-dir"))
        (succeeded (gensym "test-repo-succeeded")))
    `(let* ((,temp-dir   (make-temp-file "consult-jj-test" t))
            (default-directory ,temp-dir)
            ;; Suppress noisy test output
            (consult-jj--display-function #'ignore)
            (,succeeded nil))
       (unwind-protect
           (with-temp-buffer
             (test-sh "jj git init")
             (let ((test-repo-buffer (current-buffer)))
               (ignore test-repo-buffer) ;; For byte compile warning
               ,@body)
             (setq ,succeeded t))
         (if ,succeeded
             (delete-directory ,temp-dir t)
           (message "Test failed; temp repo kept at: %s"
                    ,temp-dir))))))

(defmacro as-temp-buffer (buffer &rest body)
  "Run BODY with the current buffer as BUFFER.

BUFFER is killed on completion.  The buffer that was current before
entering is restored afterwards, even if BUFFER is killed by BODY, so
that `default-directory' (which is buffer-local) is not left pointing
outside the test repository."
  (declare (indent 1))
  (let ((buf (gensym "buf"))
        (prev (gensym "prev")))
    `(let* ((,prev (current-buffer))
            (,buf ,buffer))
       (unwind-protect
           (with-current-buffer ,buf
             ,@body)
         (when (buffer-live-p ,buf)
           (kill-buffer ,buf))
         (when (buffer-live-p ,prev)
           (set-buffer ,prev))))))

(defmacro with-completing-read (result-fn &rest body)
  "Execute BODY with `completing-read' stubbed to call RESULT-FN.

RESULT-FN is a function invoked with the `completing-read' arguments;
its return value is used as the completion result."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read) ,result-fn))
     ,@body))

(defun test-write-file (filename contents)
  "Write CONTENTS to FILENAME, overwriting if it exists."
  (when-let ((dir (file-name-directory filename)))
    (make-directory dir t))
  (write-region contents nil filename nil 'silent))

(defun test-consult-jj-log-revisions ()
  "Return the revisions for the current repo."
  (let ((buffer (consult-jj--log)))
    (unwind-protect
        (with-current-buffer buffer
          (cl-loop for overlay in (overlays-in (point-min) (point-max))
                   for revision = (overlay-get overlay 'consult-jj--revision)
                   when revision
                   collect revision))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun test-change-id-of (rev)
  "Return the change id of REV, with surrounding JSON quotes stripped."
  (test-sh (list "jj" "log" "--no-graph" "-r" rev
                 "-T" "change_id")))

(defun test-wait-for-process (buffer)
  "Wait for the `jj' process running in BUFFER to finish.

Polls every 0.1 seconds until the process exits, so that tests can
assert on the asynchronously generated buffer contents."
  (let ((proc (get-buffer-process buffer)))
    (while (and proc (process-live-p proc))
      (sleep-for 0.1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; root
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest root-returns-root-directory ()
  (with-test-repo
    (should (string= (consult-jj-root)
                     default-directory))))

(ert-deftest root-in-subdirectory-returns-root-directory ()
  (with-test-repo
    (let ((root default-directory))
      (test-write-file "foo/bar/baz.txt" "")
      (with-current-buffer (find-file "foo/bar/baz.txt")
        (should (string= (consult-jj-root)
                         root))))))

(ert-deftest root-outside-repo-returns-error ()
  (let ((default-directory "/tmp"))
    (should-error (consult-jj-root) :type 'jj-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consult-jj-read-revision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest empty-input-returns-default-revision ()
  (with-test-repo
    (with-completing-read (lambda (&rest _) "")
      (should (string= (consult-jj-read-revision "jj new" "@some-ref")
                       "@some-ref")))))

(ert-deftest candidate-selection-returns-change-id ()
  (with-test-repo
    (let ((expected-change-id (consult-jj--rev-change-id "@")))
      (with-completing-read (lambda (_prompt table &rest _)
                              (caar table))
        (should (string= (consult-jj-read-revision "jj new" "@")
                         expected-change-id))))))

(ert-deftest custom-text-input-returns-itself ()
  (with-test-repo
    (with-completing-read (lambda (&rest _) "my-custom-revision")
      (should (string= (consult-jj-read-revision "jj edit" "@")
                       "my-custom-revision")))))

(ert-deftest outside-jj-repo-signals-jj-error ()
  (let ((default-directory "/tmp"))
    (should-error (consult-jj-read-revision "jj new" "@")
                  :type 'jj-error)))

(ert-deftest log-buffer-is-cleaned-up-after-completion ()
  (with-test-repo
    (let (log-buffer-exists-during-completion)
      (with-completing-read (lambda (&rest _)
                              (setq log-buffer-exists-during-completion
                                    (cl-find "*jj-log*" (mapcar #'buffer-name (buffer-list))
                                             :test #'string-prefix-p))
                              "")
        (consult-jj-read-revision "jj new" "@"))
      (should log-buffer-exists-during-completion)
      (should-not (cl-find "*jj-log*" (mapcar #'buffer-name (buffer-list))
                           :test #'string-prefix-p)))))

(ert-deftest prompt-includes-prefix-and-default-revision ()
  (with-test-repo
    (let (captured-prompt)
      (with-completing-read (lambda (prompt &rest _)
                              (setq captured-prompt prompt)
                              "")
        (consult-jj-read-revision "jj new" "@"))
      (should (string-match-p "jj new" captured-prompt))
      (should (string-match-p "(default @)" captured-prompt)))))

(ert-deftest matching-candidate-overlay-receives-selected-face ()
  (with-temp-buffer
    (let* ((overlay-a   (make-overlay (point-min) (point-max)))
           (overlay-b   (make-overlay (point-min) (point-max)))
           (candidates  `(("entry-a" . ,overlay-a)
                          ("entry-b" . ,overlay-b))))
      (consult-jj--read-revision-highlight-candidate "entry-a" candidates)
      (should (eq (overlay-get overlay-a 'face)
                  'consult-jj-selected-face))
      (should-not (eq (overlay-get overlay-b 'face)
                      'consult-jj-selected-face)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest new-with-change-id-creates-change-on-top-of-it ()
  (with-test-repo
    (let ((base (test-change-id-of "@")))
      (consult-jj-new base)
      (should (not (equal base (test-change-id-of "@"))))
      (should (equal base (test-change-id-of "@-"))))))

(ert-deftest new-creates-empty-change ()
  (with-test-repo
    (consult-jj-new "@")
    (should (string-empty-p
             (consult-jj--revision-description
              (car (test-consult-jj-log-revisions)))))))

(ert-deftest new-with-bookmark-creates-change-on-top-of-bookmark ()
  (with-test-repo
    (test-sh "jj bookmark set main")
    (consult-jj-new "main")
    (should (equal (test-change-id-of "main")
                   (test-change-id-of "@-")))))

(ert-deftest new-without-rev-prompts-with-prefix-and-default-at ()
  (with-test-repo
    (let ((called-with nil))
      (cl-letf (((symbol-function 'consult-jj-read-revision)
                 (lambda (prompt default)
                   (setq called-with (list prompt default))
                   "@")))
        (consult-jj-new))
      (should (equal called-with '("jj new" "@"))))))

(ert-deftest new-without-rev-runs-new-on-prompted-rev ()
  (with-test-repo
    (let ((base (test-change-id-of "@")))
      (cl-letf (((symbol-function 'consult-jj-read-revision)
                 (lambda (_prompt _default) base)))
        (consult-jj-new))
      (should (equal base (test-change-id-of "@-"))))))

(ert-deftest new-displays-command-output ()
  (with-test-repo
    (let* ((displayed nil)
           (consult-jj--display-function
            (lambda (msg) (setq displayed msg))))
      (consult-jj-new "@")
      (should (string-match-p "Working copy" displayed)))))

(ert-deftest new-outside-repo-signals-error ()
  (let ((default-directory "/tmp"))
    (should-error (consult-jj-new "@") :type 'jj-error)))

(ert-deftest new-with-invalid-revision-signals-error ()
  (with-test-repo
    (should-error (consult-jj-new "zzzznope") :type 'jj-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest edit-with-change-id-moves-working-copy-to-it ()
  (with-test-repo
    (test-sh "jj new")
    (let ((base (test-change-id-of "@-")))
      (consult-jj-edit base)
      (should (equal base (test-change-id-of "@"))))))

(ert-deftest edit-with-bookmark-moves-working-copy-to-bookmark ()
  (with-test-repo
    (test-sh "jj bookmark set main"
             "jj new")
    (consult-jj-edit "main")
    (should (equal (test-change-id-of "@")
                   (test-change-id-of "main")))))

(ert-deftest edit-without-rev-prompts-with-prefix-and-default-at ()
  (with-test-repo
    (let ((called-with nil))
      (cl-letf (((symbol-function 'consult-jj-read-revision)
                 (lambda (prompt default)
                   (setq called-with (list prompt default))
                   "@")))
        (consult-jj-edit))
      (should (equal called-with '("jj edit" "@"))))))

(ert-deftest edit-without-rev-runs-edit-on-prompted-rev ()
  (with-test-repo
    (test-sh "jj new")
    (let ((base (test-change-id-of "@-")))
      (cl-letf (((symbol-function 'consult-jj-read-revision)
                 (lambda (_prompt _default) base)))
        (consult-jj-edit))
      (should (equal base (test-change-id-of "@"))))))

(ert-deftest edit-displays-command-output ()
  (with-test-repo
    (test-sh "jj new")
    (let* ((displayed nil)
           (consult-jj--display-function
            (lambda (msg) (setq displayed msg))))
      (consult-jj-edit "@-")
      (should (string-match-p "Working copy" displayed)))))

(ert-deftest edit-outside-repo-signals-error ()
  (let ((default-directory "/tmp"))
    (should-error (consult-jj-edit "@") :type 'jj-error)))

(ert-deftest edit-with-invalid-revision-signals-error ()
  (with-test-repo
    (should-error (consult-jj-edit "zzzznope") :type 'jj-error)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest diff-at-without-rev-prompts-with-prefix-and-default-at ()
  (with-test-repo
    (let ((called-with nil))
      (cl-letf (((symbol-function 'consult-jj-read-revision)
                 (lambda (prompt default)
                   (setq called-with (list prompt default))
                   "@"))
                ((symbol-function 'consult-jj--start-process) #'ignore))
        (consult-jj-diff-at))
      (should (equal called-with '("jj diff at" "@"))))))

(ert-deftest diff-at-with-rev-does-not-prompt ()
  (with-test-repo
    (cl-letf (((symbol-function 'consult-jj-read-revision)
               (lambda (&rest _) (error "should not prompt")))
              ((symbol-function 'consult-jj--start-process) #'ignore))
      (should (consult-jj-diff-at "@")))))

(ert-deftest diff-at-returns-jj-diff-buffer ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-at "@")
      (test-wait-for-process (current-buffer))
      (should (string-prefix-p "*jj-diff*" (buffer-name (current-buffer)))))))

(ert-deftest diff-at-creates-a-new-buffer-each-time ()
  (with-test-repo
    (let ((first (consult-jj-diff-at "@")))
      (test-wait-for-process first)
      (kill-buffer first)
      (with-current-buffer test-repo-buffer
        (as-temp-buffer (consult-jj-diff-at "@")
          (test-wait-for-process (current-buffer))
          (should-not (eq first (current-buffer))))))))

(ert-deftest diff-at-shows-only-requested-revision ()
  (with-test-repo
    (test-write-file "a.txt" "one\n")
    (test-sh "jj new")
    (test-write-file "b.txt" "two\n")
    (test-sh "jj new")
    (test-write-file "c.txt" "three\n")
    (as-temp-buffer (consult-jj-diff-at "@-")
      (test-wait-for-process (current-buffer))
      (should (string= (buffer-string)
                       (concat (test-sh "jj diff -r @- --git")
                               "\n"))))))

(ert-deftest diff-at-sets-default-directory-to-repo-root ()
  (with-test-repo
    (let ((root default-directory))
      (test-write-file "foo/bar.txt" "content\n")
      (as-temp-buffer (find-file "foo/bar.txt")
        (as-temp-buffer (consult-jj-diff-at "@")
          (test-wait-for-process (current-buffer))
          (should (string= default-directory root)))))))

(ert-deftest diff-at-pops-to-jj-diff-buffer ()
  (with-test-repo
    (let (popped)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _) (setq popped buffer))))
        (as-temp-buffer (consult-jj-diff-at "@")
          (test-wait-for-process (current-buffer))
          (should (eq popped (current-buffer))))))))

(ert-deftest diff-at-starts-asynchronous-process-running-diff-git-for-revision ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-at "abc123")
      (let ((proc (get-buffer-process (current-buffer))))
        (should (process-live-p proc))
        (test-wait-for-process (current-buffer))
        (should (equal (process-command proc)
                       '("jj"
                         "--color" "never"
                         "--no-pager"
                         "diff"
                         "--git"
                         "-r" "abc123")))))))

(ert-deftest diff-at-on-clean-repo-shows-empty-diff ()
  (with-test-repo
    (let ((buffer (consult-jj-diff-at "@")))
      (unwind-protect
          (progn
            (test-wait-for-process buffer)
            (with-current-buffer buffer
              (should (string-empty-p (buffer-string)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest populated-diff-buffer-finalize-enables-diff-mode-read-only-at-point-min ()
  (with-temp-buffer
    (insert "diff --git a/a.txt b/a.txt\n")
    (consult-jj-diff--finalize)
    (should (derived-mode-p 'diff-mode))
    (should buffer-read-only)
    (should (= (point) (point-min)))))

(ert-deftest diff-at-outside-repo-signals-error ()
  (let ((default-directory "/tmp"))
    (should-error (consult-jj-diff-at "@") :type 'jj-error)))

(ert-deftest diff-at-with-invalid-revision-shows-error-output-in-buffer ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-at "zzzznope")
      (test-wait-for-process (current-buffer))
      (should (string-match-p "Error" (buffer-string)))
      (should (derived-mode-p 'diff-mode)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Describe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest describe-without-rev-prompts-with-prefix-and-default-at ()
  (with-test-repo
    (let ((called-with nil)
          (buffer nil))
      (cl-letf (((symbol-function 'consult-jj-read-revision)
                 (lambda (prompt default)
                   (setq called-with (list prompt default))
                   "@")))
        (setq buffer (consult-jj-describe)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (should (equal called-with '("jj describe" "@"))))))

(ert-deftest describe-with-rev-does-not-prompt ()
  (with-test-repo
    (cl-letf (((symbol-function 'consult-jj-read-revision)
               (lambda (&rest _) (error "should not prompt"))))
      (as-temp-buffer (consult-jj-describe "@")
        (should (derived-mode-p 'consult-jj-describe-mode))))))

(ert-deftest describe-buffer-default-directory-is-repo-root ()
  (with-test-repo
    (let ((root default-directory))
      (as-temp-buffer (consult-jj-describe "@")
        (should (string= default-directory root))))))

(ert-deftest describe-buffer-is-in-consult-jj-describe-mode ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (should (derived-mode-p 'consult-jj-describe-mode))
      (should (derived-mode-p 'markdown-mode)))))

(ert-deftest describe-buffer-records-revision ()
  (with-test-repo
    (let ((change-id (test-change-id-of "@")))
      (as-temp-buffer (consult-jj-describe change-id)
        (should (string= consult-jj--describe-revision change-id))))))

(ert-deftest describe-buffer-header-line-shows-help-text ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (let ((header (substitute-command-keys header-line-format)))
        (should (string-match-p "Accept" header))
        (should (string-match-p "Reject" header))
        (should (string-match-p "Diff" header))))))

(ert-deftest describe-buffer-contains-revision-description ()
  (with-test-repo
    (test-sh "jj describe -r @ -m 'Hello world'")
    (as-temp-buffer (consult-jj-describe "@")
      (should (string-match-p "Hello world" (buffer-string))))))

(ert-deftest describe-buffer-for-fresh-change-has-empty-description ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (should (string-prefix-p "\nJJ:" (buffer-string))))))

(ert-deftest describe-buffer-contains-jj-change-id-comment ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (should (string-match-p "JJ: Change ID:" (buffer-string))))))

(ert-deftest describe-creates-a-new-buffer-each-time ()
  (with-test-repo
    (let ((first (consult-jj-describe "@")))
      (kill-buffer first)
      (with-current-buffer test-repo-buffer
        (as-temp-buffer (consult-jj-describe "@")
          (should-not (eq first (current-buffer))))))))

(ert-deftest describe-outside-repo-signals-error ()
  (let ((default-directory "/tmp"))
    (should-error (consult-jj-describe "@") :type 'jj-error)))

(ert-deftest describe-accept-strips-jj-comment-lines-before-sending ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (let ((sent nil))
        (goto-char (point-max))
        (insert "my new description\n")
        (cl-letf (((symbol-function 'call-process-region)
                   (lambda (start end &rest _)
                     (setq sent (buffer-substring-no-properties start end))
                     0)))
          (consult-jj-describe-accept))
        (should (string= sent "my new description"))
        (should-not (string-match-p "^JJ:" sent))))))

(ert-deftest describe-accept-strips-leading-and-trailing-blank-lines ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (let ((sent nil))
        (goto-char (point-max))
        (insert "\n\nmy description\n\n\n")
        (cl-letf (((symbol-function 'call-process-region)
                   (lambda (start end &rest _)
                     (setq sent (buffer-substring-no-properties start end))
                     0)))
          (consult-jj-describe-accept))
        (should (string= sent "my description"))))))

(ert-deftest describe-accept-updates-revision-description-in-repo ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (goto-char (point-max))
      (insert "new description text\n")
      (consult-jj-describe-accept))
    (should (string= (test-sh "jj log --no-graph -r @ -T description")
                     "new description text"))))

(ert-deftest describe-accept-kills-buffer ()
  (with-test-repo
    (let ((buffer (consult-jj-describe "@")))
      (with-current-buffer buffer
        (consult-jj-describe-accept))
      (should-not (buffer-live-p buffer)))))

(ert-deftest describe-accept-displays-description-updated-message ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (let ((displayed nil))
        (let ((consult-jj--display-function
               (lambda (msg) (setq displayed msg))))
          (consult-jj-describe-accept))
        (should (string-match-p "Description updated" displayed))))))

(ert-deftest describe-reject-does-not-change-description ()
  (with-test-repo
    (test-sh "jj describe -r @ -m 'original description'")
    (let (buffer)
      (as-temp-buffer (setq buffer (consult-jj-describe "@"))
        (goto-char (point-max))
        (insert "changed description\n")
        (consult-jj-describe-reject)
        (should-not (buffer-live-p buffer)))
      (should (string= (test-sh "jj log --no-graph -r @ -T description")
                       "original description")))))

(ert-deftest describe-diff-shows-diff-for-described-revision ()
  (with-test-repo
    (let ((change-id (test-change-id-of "@")))
      (as-temp-buffer (consult-jj-describe change-id)
        (let ((diffed nil))
          (cl-letf (((symbol-function 'consult-jj-diff-at)
                     (lambda (rev) (setq diffed rev))))
            (consult-jj-describe-diff))
          (should (string= diffed change-id)))))))

(ert-deftest describe-diff-signals-error-outside-describe-buffer ()
  (with-test-repo
    (with-temp-buffer
      (should-error (consult-jj-describe-diff) :type 'user-error))))

(ert-deftest describe-diff-signals-error-when-revision-is-nil ()
  (with-test-repo
    (with-temp-buffer
      (consult-jj-describe-mode)
      (should-error (consult-jj-describe-diff) :type 'user-error))))

(ert-deftest diff-from-without-rev-prompts-with-prefix-and-default-at-minus ()
  (with-test-repo
    (let ((called-with nil))
      (cl-letf (((symbol-function 'consult-jj-read-revision)
                 (lambda (prompt default)
                   (setq called-with (list prompt default))
                   "@"))
                ((symbol-function 'consult-jj--start-process) #'ignore))
        (consult-jj-diff-from))
      (should (equal called-with '("jj diff from" "@-"))))))

(ert-deftest diff-from-with-rev-does-not-prompt ()
  (with-test-repo
    (cl-letf (((symbol-function 'consult-jj-read-revision)
               (lambda (&rest _) (error "should not prompt")))
              ((symbol-function 'consult-jj--start-process) #'ignore))
      (should (consult-jj-diff-from "@")))))

(ert-deftest diff-from-returns-jj-diff-buffer ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-from "@")
      (test-wait-for-process (current-buffer))
      (should (string-prefix-p "*jj-diff*" (buffer-name (current-buffer)))))))

(ert-deftest diff-from-creates-a-new-buffer-each-time ()
  (with-test-repo
    (let ((first       (consult-jj-diff-from "@")))
      (test-wait-for-process first)
      (kill-buffer first)
      (with-current-buffer test-repo-buffer
        (as-temp-buffer (consult-jj-diff-from "@")
          (test-wait-for-process (current-buffer))
          (should-not (eq first (current-buffer))))))))

(ert-deftest diff-from-starts-asynchronous-process-running-diff-git-from-revision ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-from "abc123")
      (let ((proc (get-buffer-process (current-buffer))))
        (should (process-live-p proc))
        (test-wait-for-process (current-buffer))
        (should (equal (process-command proc)
                       '("jj"
                         "--color" "never"
                         "--no-pager"
                         "diff"
                         "--git"
                         "--from" "abc123")))))))

(ert-deftest diff-from-on-clean-repo-shows-empty-diff ()
  (with-test-repo
    (let ((buffer (consult-jj-diff-from "@")))
      (unwind-protect
          (progn
            (test-wait-for-process buffer)
            (with-current-buffer buffer
              (should (string-empty-p (buffer-string)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest diff-from-shows-changes-from-revision-to-working-copy ()
  (with-test-repo
    (test-write-file "a.txt" "one\n")
    (test-sh "jj new")
    (test-write-file "b.txt" "two\n")
    (as-temp-buffer (consult-jj-diff-from "@-")
      (test-wait-for-process (current-buffer))
      (should (string= (buffer-string)
                       (concat (test-sh "jj diff --git --from @-")
                               "\n"))))))

(ert-deftest diff-from-pops-to-jj-diff-buffer ()
  (with-test-repo
    (let (popped)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _) (setq popped buffer))))
        (as-temp-buffer (consult-jj-diff-from "@")
          (test-wait-for-process (current-buffer))
          (should (eq popped (current-buffer))))))))

(ert-deftest diff-from-sets-default-directory-to-repo-root ()
  (with-test-repo
    (let ((root default-directory))
      (test-write-file "foo/bar.txt" "content\n")
      (as-temp-buffer (find-file "foo/bar.txt")
        (as-temp-buffer (consult-jj-diff-from "@")
          (test-wait-for-process (current-buffer))
          (should (string= default-directory root)))))))

(ert-deftest diff-from-outside-repo-signals-error ()
  (let ((default-directory "/tmp"))
    (should-error (consult-jj-diff-from "@") :type 'jj-error)))

(ert-deftest diff-from-with-invalid-revision-shows-error-output-in-buffer ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-from "zzzznope")
      (test-wait-for-process (current-buffer))
      (should (string-match-p "Error" (buffer-string)))
      (should (derived-mode-p 'diff-mode)))))
