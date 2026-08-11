;; -*- lexical-binding: t -*-

(require 'ert)
(require 'consult-jj)
(require 'consult-jj-diff)
(require 'consult-jj-describe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-sh (&rest commands)
  "Run all shell commands in COMMANDS.

Each element of COMMANDS must either be:
- A string
- A list of strings, that are concatenated to form a single command.

Each command is run synchronously. Fails if any of the commands fails."
  (with-temp-buffer
    (dolist (cmd commands)
      (let ((cmd-str (if (stringp cmd) cmd (string-join cmd " "))))
        (erase-buffer)
        (should (zerop (call-process shell-file-name nil t nil
                                     shell-command-switch cmd-str)))))
    (buffer-string)))

(defmacro with-test-repo (&rest body)
  "Run BODY in a new jj repository.

During execution, the following variables are bound:

`test-repo-directory' - The root directory of the test repo.
`test-repo-buffer' - A temporary buffer where `default-directory' is at the root
                     of the repo.

The jj repository is deleted upon successful completion. However, it is retained
(for debugging purposes) if there is a failure."
  (declare (indent 0) (debug body))
  (let ((succeeded (gensym "test-repo-succeeded")))
    `(let* ((test-repo-directory (make-temp-file "consult-jj-test" t))
            (default-directory   test-repo-directory)
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
             (delete-directory test-repo-directory t)
           (message "Test failed; temp repo kept at: %s"
                    test-repo-directory))))))

(defmacro as-temp-buffer (buffer &rest body)
  "Run BODY with current buffer as BUFFER and kill BUFFER when done.

Upon completion, the previous current buffer is restored."
  (declare (indent 1))
  (let ((buf (gensym "buf"))
        (prev (gensym "prev")))
    `(let* ((,prev (current-buffer))
            (,buf ,buffer))
       (unwind-protect
           (with-current-buffer ,buf
             ,@body)
         (when-let (proc (get-buffer-process ,buf))
           (kill-process proc))
         (when (buffer-live-p ,buf)
           (kill-buffer ,buf))
         (when (buffer-live-p ,prev)
           (set-buffer ,prev))))))

(defun test-write-file (filename contents)
  "Sets the contents of FILENAME to CONTENTS.

Overwrites the contents if they exist."
  (when-let ((dir (file-name-directory filename)))
    (make-directory dir t))
  (write-region contents nil filename nil 'silent))

(defun test-jj-change-id (revision)
  "Return the change ID for REVISION."
  (with-temp-buffer
    (should (zerop (call-process "jj" nil t nil "log" "-r" revision
                                 "--no-graph" "-T" "change_id")))
    (string-trim (buffer-string))))

(defun test-jj-description (revision)
  "Return the description of REVISION."
  (with-temp-buffer
    (let* ((result (call-process "jj" nil t nil "log" "-r" revision
                                 "--no-graph" "-T" "description"))
           (output (buffer-string)))
      (unless (zerop result)
        (error "jj log failed with exit code %d:\n%s" result output))
      (string-trim output))))


(defun test-wait-for-process (&optional buffer)
  "Wait for the `jj' process running in BUFFER to finish.

Polls every 100ms until the process exits, so that tests can assert on the
asynchronously generated buffer contents.  Signals an error if the process does
not exit in time."
  (let* ((buffer         (or buffer (current-buffer)))
         (proc           (get-buffer-process buffer))
         (sleep-duration 0.1)
         (timeout        10)
         (remaining      (ceiling (/ timeout sleep-duration))))
    (while (and proc (process-live-p proc) (> remaining 0))
      (sleep-for sleep-duration)
      (setq remaining (1- remaining)))
    (when (and proc (process-live-p proc))
      (error "Process %S did not finish within %s seconds"
             proc timeout))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; root
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest root-returns-root-directory ()
  (with-test-repo
    (should (string= test-repo-directory
                     (consult-jj-root)))))

(ert-deftest root-in-subdirectory-returns-root-directory ()
  (with-test-repo
    (test-write-file "foo/bar/baz.txt" "")
    (as-temp-buffer (find-file "foo/bar/baz.txt")
      (should (string= test-repo-directory
                       (consult-jj-root))))))

(ert-deftest root-outside-of-repo-is-error ()
  (let ((temp-dir (make-temp-file "consult-jj-outside-repo" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          (with-temp-buffer
            (should-error (consult-jj-root) :type 'jj-error)))
      (delete-directory temp-dir t))))

;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; read revision
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest read-revision-returns-all-revisions ()
  (with-test-repo
    (test-sh
     "jj new")
    (let* ((got-collection nil)
           (completing-read-function (lambda (_ collection &rest _)
                                       (setq got-collection collection)
                                       "")))
      (consult-jj-read-revision "" "")
      (cl-destructuring-bind (first second third) got-collection
        ;; Newest working copy
        (should (stringp (car first)))
        (should (overlayp (cdr first)))
        ;; Old working copy
        (should (stringp (car second)))
        (should (overlayp (cdr second)))
        ;; Root
        (should (stringp (car third)))
        (should (overlayp (cdr third)))))))

(ert-deftest read-revision-contains-change-id-description-tags ()
  (with-test-repo
    (test-sh
     "printf 'First Line\nSecondLine\n' | jj describe --stdin"
     "jj bookmark set bookmark1"
     "jj bookmark set bookmark2")
    (let* ((got-collection nil)
           (completing-read-function (lambda (_ collection &rest _)
                                       (setq got-collection collection)
                                       "")))
      (consult-jj-read-revision "" "")
      (let ((entry (substring-no-properties (caar got-collection))))
        (should (string-match-p "\\`[a-z]\\{8\\}\\s-+First Line\\s-+bookmark1 bookmark2\\'" entry))))))


(ert-deftest read-revision-uses-prompt-prefix-and-default ()
  (with-test-repo
    (let* ((got-prompt nil)
           (completing-read-function (lambda (prompt _ &rest _)
                                       (setq got-prompt prompt)
                                       "")))
      (consult-jj-read-revision "my-prefix" "default-rev")
      (should (equal "my-prefix revision (default default-rev): " got-prompt)))))

(ert-deftest read-revision-empty-string-selected-returns-default ()
  (with-test-repo
    (test-sh
     "printf 'First Line\nSecondLine\n' | jj describe --stdin"
     "jj bookmark set bookmark1"
     "jj bookmark set bookmark2")
    (let* ((completing-read-function (lambda (_ _ &rest _)
                                       "Completing read that returns empty string."
                                       "")))
      (should (equal "the-default"
                     (consult-jj-read-revision "" "the-default"))))))

(ert-deftest read-revision-non-matching-returns-raw-string ()
  (with-test-repo
    (let* ((completing-read-function (lambda (_ _ &rest _)
                                       "Completing read that returns empty string."
                                       "totally-custom-string")))
      (should (equal "totally-custom-string"
                     (consult-jj-read-revision "" "the-default"))))))

(ert-deftest read-revision-candidate-returns-change-id ()
  (with-test-repo
    (let* ((selected-overlay nil)
           (completing-read-function (lambda (_ collection &rest _)
                                       "Completing read that returns empty string."
                                       (let ((selected (car collection)))
                                         (setq selected-overlay (cdr selected))
                                         (car selected))))
           (result (consult-jj-read-revision "" "the-default"))
           ;; Note: expected can only be generated after
           ;; consult-jj-read-revision has run and populated selected-overlay.
           (expected (consult-jj--revision-change-id
                      (overlay-get selected-overlay 'consult-jj--revision))))
      (should (stringp result))
      (should (equal result expected)))))


;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; new
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest new-creates-new-revision ()
  (with-test-repo
    (let ((initial-change-id (test-jj-change-id "@")))
      (consult-jj-new "@")
      (should-not (equal initial-change-id
                         (test-jj-change-id "@")))
      (should (equal initial-change-id
                     (test-jj-change-id "@-"))))))

(ert-deftest new-on-bookmark-adds-child-to-bookmarked-revision ()
  (with-test-repo
    (test-sh
     "jj bookmark create bookmark1"
     "jj new")
    (let ((bookmark-change-id     (test-jj-change-id "bookmark1"))
          (working-copy-change-id (test-jj-change-id "@")))
      (consult-jj-new "bookmark1")
      (should (equal bookmark-change-id
                     (test-jj-change-id "@-")))
      (should-not (equal working-copy-change-id
                         (test-jj-change-id "@")))
      (should (equal bookmark-change-id
                     (test-jj-change-id "bookmark1"))))))

(ert-deftest new-errors-on-unresolvable-revision ()
  (with-test-repo
    (should-error (consult-jj-new "no-such-revision")
                  :type 'jj-error)))

;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; edit
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest edit-moves-working-copy-to-specified-revision ()
  (with-test-repo
    (test-sh
     "jj new"
     "printf 'Edit me\n' | jj describe --stdin")
    (let* ((new-change-id    (test-jj-change-id "@"))
           (parent-change-id (test-jj-change-id "@-")))
      (consult-jj-edit parent-change-id)
      (should (equal parent-change-id
                     (test-jj-change-id "@")))
      (should-not (equal new-change-id
                         (test-jj-change-id "@"))))))

(ert-deftest edit-of-current-working-copy-revision-succeeds ()
  (with-test-repo
    (let ((change-id (test-jj-change-id "@")))
      (consult-jj-edit "@")
      (should (equal change-id
                     (test-jj-change-id "@"))))))

(ert-deftest edit-errors-on-unresolvable-revision ()
  (with-test-repo
    (should-error (consult-jj-edit "no-such-revision")
                  :type 'jj-error)))


;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; describe
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest describe-creates-buffer-with-mode-and-description ()
  (with-test-repo
    (test-sh
     "printf 'First Line\nSecondLine\n' | jj describe --stdin")
    (as-temp-buffer (consult-jj-describe "@")
      (should (derived-mode-p 'consult-jj-describe-mode))
      (should (string-prefix-p "First Line\nSecondLine\n\n"
                               (buffer-string)))
      (should (string-match-p "^JJ: Change ID: "
                              (buffer-string)))
      (should (string-match-p "^JJ: Lines starting with \"JJ:\" (like this one) will be removed.\n"
                              (buffer-string))))))

(ert-deftest describe-creates-buffer-with-change-id-header ()
  (with-test-repo
    (let ((change-id-shortest (with-temp-buffer
                                (should (zerop (call-process "jj" nil t nil
                                                             "log" "-r" "@"
                                                             "--no-graph"
                                                             "-T" "change_id.shortest(8)")))
                                (buffer-string))))
      (as-temp-buffer (consult-jj-describe "@")
        (should (string-match-p (concat "^JJ: Change ID: "
                                        change-id-shortest)
                                (buffer-string)))))))

(ert-deftest describe-on-unresolvable-revision-is-error ()
  (with-test-repo
    (should-error (consult-jj-describe "no-such-revision")
                  :type 'jj-error)))

(ert-deftest describe-accept-sets-description-on-revision ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (delete-region (point-min) (point-max))
      (insert "This is the new description")
      (consult-jj-describe-accept))
    (should (equal "This is the new description"
                   (test-jj-description "@")))))

(ert-deftest describe-accept-strips-jj-comment-lines ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (delete-region (point-min) (point-max))
      (insert "New Description\nJJ: Comment to strip\nSecond Line")
      (consult-jj-describe-accept))
    (should (equal "New Description\nSecond Line"
                   (test-jj-description "@")))))

(ert-deftest describe-accept-trims-leading-and-trailing-blank-lines ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (delete-region (point-min) (point-max))
      (insert "\n\nDescription\n\n")
      (consult-jj-describe-accept))
    (should (equal "Description"
                   (test-jj-description "@")))))

(ert-deftest describe-accept-outside-describe-buffer-is-error ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (delete-region (point-min) (point-max))
      (insert "Description")
      (with-temp-buffer
        (should-error (consult-jj-describe-accept)
                      :type 'user-error)))))


(ert-deftest describe-reject-kills-buffer-without-saving-description ()
  (with-test-repo
   (let ((describe-buffer (consult-jj-describe "@")))
     (as-temp-buffer describe-buffer
                     (delete-region (point-min) (point-max))
                     (insert "Modified Description\n")
                     (consult-jj-describe-reject)
                     (should-not (buffer-live-p describe-buffer))
                     (should (equal "" (test-jj-description "@")))))))

(ert-deftest describe-reject-outside-describe-buffer-is-error ()
  (with-test-repo
    (with-temp-buffer
      (should-error (consult-jj-describe-reject)
                    :type 'user-error))))

(ert-deftest describe-diff-shows-diff-of-revision ()
  (with-test-repo
    (test-write-file "file.txt" "content\nmore content\n")
    (as-temp-buffer (consult-jj-describe "@")
      (as-temp-buffer (consult-jj-describe-diff)
        (test-wait-for-process)
        (should (eq major-mode 'diff-mode))
        (should (equal (buffer-string)
                       "diff --git a/file.txt b/file.txt
new file mode 100644\nindex 0000000000..86436d0dd5
--- /dev/null
+++ b/file.txt
@@ -0,0 +1,2 @@
+content
+more content
"))))))

(ert-deftest describe-diff-on-empty-is-empty ()
  (with-test-repo
    (as-temp-buffer (consult-jj-describe "@")
      (as-temp-buffer (consult-jj-describe-diff)
        (test-wait-for-process)
        (should (eq major-mode 'diff-mode))
        (should (equal (buffer-string) ""))))))

(ert-deftest describe-diff-outside-describe-buffer-is-error ()
  (with-test-repo
    (with-temp-buffer
      (should-error (consult-jj-describe-diff)
                    :type 'user-error))))


;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; diff at
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest diff-at-creates-jj-diff-buffer ()
  (with-test-repo
    (test-write-file "file.txt" "content\n")
    (as-temp-buffer (consult-jj-diff-at "@")
      (test-wait-for-process)
      (should (string-prefix-p "*jj-diff*" (buffer-name)))
      (should (derived-mode-p 'diff-mode)))))

(ert-deftest diff-at-shows-diff-of-revision ()
  (with-test-repo
    (test-write-file "file-a.txt" "target content\n")
    (test-sh "jj bookmark set target")
    (test-sh "jj new")
    (test-write-file "file-b.txt" "other\n")
    (test-sh "jj bookmark set other")
    (as-temp-buffer (consult-jj-diff-at "target")
      (test-wait-for-process)
      (should (eq major-mode 'diff-mode))
      (should (equal "diff --git a/file-a.txt b/file-a.txt
new file mode 100644
index 0000000000..2ceb84c5b3
--- /dev/null
+++ b/file-a.txt
@@ -0,0 +1,1 @@
+target content
" (buffer-string))))))

(ert-deftest diff-at-shows-diff-of-empty-revision-is-empty ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-at "@")
      (test-wait-for-process)
      (should (eq major-mode 'diff-mode))
      (should (equal (buffer-string) "")))))

(ert-deftest diff-at-on-unresolvable-revision-shows-error-in-buffer ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-at "no-such-revision")
      (test-wait-for-process)
      (should-not (derived-mode-p 'diff-mode))
      (should (equal "Error: Revision `no-such-revision` doesn't exist\n"
                     (buffer-string))))))

(ert-deftest diff-at-navigates-to-buffer ()
  (with-test-repo
    (test-write-file "file.txt" "content\n")
    (let ((diff-buffer (consult-jj-diff-at "@")))
      (test-wait-for-process diff-buffer)
      (unwind-protect
          (should (eq diff-buffer (current-buffer)))
        (kill-buffer diff-buffer)))))


;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; diff from
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest diff-from-creates-jj-diff-buffer ()
  (with-test-repo
    (test-write-file "file.txt" "content\n")
    (as-temp-buffer (consult-jj-diff-from "root()")
      (test-wait-for-process)
      (should (string-prefix-p "*jj-diff*" (buffer-name)))
      (should (derived-mode-p 'diff-mode)))))

(ert-deftest diff-from-shows-diff-of-revision ()
  (with-test-repo
    ;; rev1
    (test-write-file "file-a.txt" "target content\n")
    (test-sh "jj bookmark set rev1")
    ;; rev2
    (test-sh "jj new")
    (test-write-file "file-b.txt" "other\n")
    ;; rev3
    (test-sh "jj new")
    (test-write-file "file-c.txt" "this one too\n")
    (as-temp-buffer (consult-jj-diff-from "rev1")
      (test-wait-for-process)
      (should (eq major-mode 'diff-mode))
      (should (equal "diff --git a/file-b.txt b/file-b.txt
new file mode 100644
index 0000000000..e45c9c2666
--- /dev/null
+++ b/file-b.txt
@@ -0,0 +1,1 @@
+other
diff --git a/file-c.txt b/file-c.txt
new file mode 100644
index 0000000000..b28a266836
--- /dev/null
+++ b/file-c.txt
@@ -0,0 +1,1 @@
+this one too
"
                     (buffer-string))))))

(ert-deftest diff-from-with-arg-to-revision-shows-diff-between-revisions ()
  (with-test-repo
    ;; rev1
    (test-write-file "file-a.txt" "first\n")
    (test-sh "jj bookmark set rev1")
    ;; rev2
    (test-sh "jj new")
    (test-write-file "file-b.txt" "second\n")
    (test-sh "jj bookmark set rev2")
    ;; rev3
    (test-sh "jj new")
    (test-write-file "file-c.txt" "third\n")
    ;; test
    (as-temp-buffer (consult-jj-diff-from "rev1" "rev2")
      (test-wait-for-process)
      (should (eq major-mode 'diff-mode))
      (should (equal "diff --git a/file-b.txt b/file-b.txt
new file mode 100644
index 0000000000..e019be006c
--- /dev/null
+++ b/file-b.txt
@@ -0,0 +1,1 @@
+second
" (buffer-string))))))

(ert-deftest diff-from-empty-diff-is-empty ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-from "@")
      (test-wait-for-process)
      (should (eq major-mode 'diff-mode))
      (should (equal (buffer-string) "")))))

(ert-deftest diff-from-on-unresolvable-revision-shows-error-in-buffer ()
  (with-test-repo
    (as-temp-buffer (consult-jj-diff-from "no-such-revision")
      (test-wait-for-process)
      (should-not (derived-mode-p 'diff-mode))
      (should (equal "Error: Revision `no-such-revision` doesn't exist\n"
                     (buffer-string))))))

(ert-deftest diff-from-navigates-to-buffer ()
  (with-test-repo
    (test-write-file "file.txt" "content\n")
    (let ((diff-buffer (consult-jj-diff-from "root()")))
      (test-wait-for-process diff-buffer)
      (unwind-protect
          (should (eq diff-buffer (current-buffer)))
        (kill-buffer diff-buffer)))))
