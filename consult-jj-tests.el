;; -*- lexical-binding: t -*-

(require 'ert)
(require 'consult-jj)

(defvar consult-jj-describe-mode-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consult-jj-test-sh (&rest commands)
  "Run shell COMMAND and additional COMMANDS, asserting each succeeds.

Each command may be a string or a list of strings (which are concatenated).
Runs synchronously in `default-directory'; output is discarded."
  (dolist (cmd commands)
    (let ((cmd-str (if (stringp cmd) cmd (string-join cmd " "))))
      (should (zerop (call-process shell-file-name nil nil nil
                                   shell-command-switch cmd-str))))))

(defmacro with-test-jj-repo (&rest body)
  `(let ((default-directory (make-temp-file "consult-jj-test" t)))
     (consult-jj-test-sh "jj git init")
     ,@body))


(defun consult-jj-test-write-file (filename contents)
  "Write CONTENTS to FILENAME, overwriting if it exists."
  (write-region contents nil filename nil 'silent))

(defun consult-jj-test-revision-json (fields)
  "Serialize revision metadata FIELDS as JSON, mimicking jj log output.

This is the inverse of `consult-jj-test-fields-to-revision'."
  (mapconcat (lambda (field-spec)
               (let ((pair (assq (car field-spec) fields)))
                 (unless pair
                   (error "Missing revision field %s" (car field-spec)))
                 (json-serialize (cdr pair))))
             consult-jj--revision-fields
             ""))

(defun consult-jj-test-fields-to-revision (fields)
  "Build a `consult-jj--revision' from FIELDS.

This is the inverse of `consult-jj-test-revision-json'."
  (apply #'consult-jj--make-revision
         (cl-loop for (field . value) in fields
                  append (list field value))))

(defun consult-jj-test-buffer-string (buffer)
  "Get the string for `buffer'.

Waits for all processes in buffer to terminate before getting the string."
  (with-current-buffer buffer
    (when-let ((proc (get-buffer-process (current-buffer))))
      (while (process-live-p proc)
        (sleep-for 0.01)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun consult-jj-test-description (rev &optional directory)
  "Get the description of REV from jj.

Runs jj in DIRECTORY, or in `default-directory' if DIRECTORY is nil."
  (with-temp-buffer
    (let ((default-directory (or directory default-directory)))
      (call-process consult-jj-executable nil t nil
                    "--color" "never" "--no-pager"
                    "log" "--no-graph" "-r" rev "-T" "description"))
    (buffer-string)))

(defvar consult-jj-test-message nil
  "Message captured by the `display-message-or-buffer' stub.")

(defun consult-jj-test-display-message-or-buffer (msg)
  "Capture MSG in `consult-jj-test-message'."
  (setq consult-jj-test-message msg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest consult-jj-root ()
  ;; when not inside a jj repo, then consult-jj-root signals a jj-error
  (let ((default-directory (make-temp-file "root-error" t)))
    (should-error (consult-jj-root)
                  :type 'jj-error))
  ;; when inside a jj repo, then consult-jj-root returns the repo root
  (with-test-jj-repo
   (should (string-equal (consult-jj-root)
                         default-directory))))

(ert-deftest consult-jj--log ()
  (with-test-jj-repo
   (consult-jj-test-sh
    "jj bookmark set mybookmark"
    "jj commit -m initial-commit"
    "jj commit -m second-commit"
    "jj new mybookmark")
   (let* ((buffer (consult-jj--log))
          (result (consult-jj-test-buffer-string buffer)))
     (should (string-match-p
              ;; Full example:
              ;; 1 @  xruwxlww will@wmedrano.dev 2026-08-07 21:40:17 cdbf09e5
              ;; 2 │  (empty) (no description set)
              ;; 3 │ ○  txllxlzn will@wmedrano.dev 2026-08-07 21:40:17 8435b9a3
              ;; 4 ├─╯  (empty) second-commit
              ;; 5 ○  yyosukvp will@wmedrano.dev 2026-08-07 21:40:17 mybookmark 67934430
              ;; 6 │  (empty) initial-commit
              ;; 7 ◆  zzzzzzzz root() 00000000
              (rx bol
                  ;; Line 1
                  "@  " (1+ alnum) " " (1+ (not (any " "))) " "
                  (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit) " "
                  (repeat 2 digit) ":" (repeat 2 digit) ":" (repeat 2 digit) " "
                  (repeat 8 hex) eol
                  ;; Line 2
                  "\n│  (empty) (no description set)\n"
                  ;; Line 3
                  "│ ○  " (1+ alnum) " " (1+ (not (any " "))) " "
                  (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit) " "
                  (repeat 2 digit) ":" (repeat 2 digit) ":" (repeat 2 digit) " "
                  (repeat 8 hex) eol
                  ;; Line 4
                  "\n├─╯  (empty) second-commit\n"
                  ;; Line 5
                  "○  " (1+ alnum) " " (1+ (not (any " "))) " "
                  (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit) " "
                  (repeat 2 digit) ":" (repeat 2 digit) ":" (repeat 2 digit) " "
                  "mybookmark " (repeat 8 hex) eol
                  ;; Line 6
                  "\n│  (empty) initial-commit\n"
                  ;; Line 7
                  "◆  zzzzzzzz root() 00000000" eol)
              result)))))

(ert-deftest consult-jj--log-finalize ()
  ;; when the log contains JSON metadata, then the metadata is removed from
  ;; the display and attached to overlays on the corresponding revisions
  (let ((rev1-fields '((:change-id          . "abc123")
                       (:change-id-shortest . "abc")
                       (:commit-id          . "aaa111")
                       (:description        . "first commit")
                       (:bookmarks          . ["mybookmark"])))
        (rev2-fields '((:change-id          . "def456")
                       (:change-id-shortest . "def")
                       (:commit-id          . "bbb222")
                       (:description        . "second commit")
                       (:bookmarks          . []))))
    (with-temp-buffer
      (insert
       "@  " (consult-jj-test-revision-json rev1-fields)
       "abc123 author 2024-01-01 00:00:00\n"
       "│  first commit\n"
       "○  " (consult-jj-test-revision-json rev2-fields)
       "def456 author 2024-01-01 00:00:00\n"
       "│  second commit\n")
      (consult-jj--log-finalize)
      (let ((revisions (mapcar
                        (lambda (overlay) (overlay-get overlay 'consult-jj--revision))
                        (overlays-in (point-min) (point-max)))))
        (should (string-equal
                 (buffer-string)
                 "@  abc123 author 2024-01-01 00:00:00\n│  first commit\n○  def456 author 2024-01-01 00:00:00\n│  second commit\n"))
        (should buffer-read-only)
        (should (equal
                 revisions
                 (list (consult-jj-test-fields-to-revision rev1-fields)
                       (consult-jj-test-fields-to-revision rev2-fields))))))))

(ert-deftest consult-jj--log-candidates-no-description ()
  ;; when a revision has no description, then the candidate shows
  ;; "(no description set)" styled with font-lock-comment-face
  (with-temp-buffer
    (insert
     "@  " (consult-jj-test-revision-json
            '((:change-id          . "abc12345")
              (:change-id-shortest . "abc")
              (:commit-id          . "aaa111")
              (:description        . "")
              (:bookmarks          . [])))
     "abc123 author 2024-01-01 00:00:00\n"
     "│\n")
    (consult-jj--log-finalize)
    (let* ((candidates (consult-jj--log-candidates (current-buffer)))
           (candidate (car candidates)))
      (should (= (length candidates) 1))
      (let ((pos (string-search "(no description set)" (car candidate))))
        (should pos)
        (should (eq (get-text-property pos 'face (car candidate))
                    'font-lock-comment-face))))))



(ert-deftest consult-jj--log-candidates ()
  ;; when the log contains revision overlays, then candidates maps display
  ;; text to the corresponding overlays
  (with-test-jj-repo
   (consult-jj-test-write-file "file.txt" "contents")
   (consult-jj-test-sh
    "jj bookmark set mybookmark"
    "jj commit -m first-commit")
   (let* ((buffer (consult-jj--log))
          (candidates (consult-jj--log-candidates buffer))
          (commit (nth 1 candidates)))
     (should (= (length candidates) 3)) ;; Contains root, commit, working-copy
     ;; the second candidate maps text to an overlay carrying revision metadata
     (should (overlayp (cdr commit)))
     (should (consult-jj--revision-p
              (overlay-get (cdr commit) 'consult-jj--revision)))
     ;; the text is the change id (8 chars), description, and bookmarks
     (should (string-match-p
              (rx bol (repeat 8 (any alnum)) (0+ " " (1+ (not (any "\n")))) eol)
              (car commit)))
     ;; the change id is styled with consult-jj-change-id-face
     (should (eq (get-text-property 0 'face (car commit))
                 'consult-jj-change-id-face))
     ;; the bookmarks are styled with consult-jj-bookmark-face
     (let ((bookmark-pos (string-search "mybookmark" (car commit))))
       (should bookmark-pos)
       (should (eq (get-text-property bookmark-pos 'face (car commit))
                   'consult-jj-bookmark-face))))))

(ert-deftest consult-jj--read-revision-highlight-candidate ()
  ;; when the candidate matches a log entry,
  ;; then that entry's overlay is given the consult-jj-selected-face face and
  ;; the other entries' overlays are cleared
  (with-temp-buffer
    (let* ((overlay-a   (make-overlay 1 1))
           (overlay-b   (make-overlay 1 1))
           (candidates  (list (cons "aaa111 first" overlay-a)
                              (cons "bbb222 second" overlay-b))))
      (consult-jj--read-revision-highlight-candidate "aaa111 first" candidates)
      (should (eq (overlay-get overlay-a 'face) 'consult-jj-selected-face))
      (should-not (overlay-get overlay-b 'face))))

  ;; when the selection moves to a different candidate,
  ;; then the previously highlighted entry loses its face and the newly
  ;; selected entry is highlighted
  (with-temp-buffer
    (let* ((overlay-a   (make-overlay 1 1))
           (overlay-b   (make-overlay 1 1))
           (candidates  (list (cons "aaa111 first" overlay-a)
                              (cons "bbb222 second" overlay-b))))
      (consult-jj--read-revision-highlight-candidate "aaa111 first" candidates)
      (consult-jj--read-revision-highlight-candidate "bbb222 second" candidates)
      (should-not (overlay-get overlay-a 'face))
      (should (eq (overlay-get overlay-b 'face) 'consult-jj-selected-face))))

  ;; when the candidate is nil,
  ;; then no overlay receives a face
  (with-temp-buffer
    (let* ((overlay-a   (make-overlay 1 1))
           (overlay-b   (make-overlay 1 1))
           (candidates  (list (cons "aaa111 first" overlay-a)
                              (cons "bbb222 second" overlay-b))))
      (consult-jj--read-revision-highlight-candidate nil candidates)
      (should-not (overlay-get overlay-a 'face))
      (should-not (overlay-get overlay-b 'face))))

  ;; when the selected text is not in the candidates alist,
  ;; then the highlight is left unchanged
  ;;
  ;; This is unreachable through vertico: it returns nil when the input
  ;; matches no candidate.  It exercises the function's defensive contract
  ;; directly.
  (with-temp-buffer
    (let* ((overlay-a   (make-overlay 1 1))
           (overlay-b   (make-overlay 1 1))
           (candidates  (list (cons "aaa111 first" overlay-a)
                              (cons "bbb222 second" overlay-b))))
      (consult-jj--read-revision-highlight-candidate "aaa111 first" candidates)
      (consult-jj--read-revision-highlight-candidate "zzz custom text" candidates)
      (should (eq (overlay-get overlay-a 'face) 'consult-jj-selected-face))
      (should-not (overlay-get overlay-b 'face))))

  ;; when there are no candidates,
  ;; then the highlight is a no-op
  (with-temp-buffer
    (consult-jj--read-revision-highlight-candidate nil nil)
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest consult-jj-read-revision ()
  ;; when the user enters empty input, then the default revision is returned
  (with-test-jj-repo
   (consult-jj-test-sh "jj commit -m first-commit")
   (cl-letf (((default-value 'completing-read-function)
              (lambda (&rest _) "")))
     (should (string-equal (consult-jj-read-revision "jj diff at" "@-")
                           "@-"))))

  ;; when the user selects a candidate, then the change id is returned
  (with-test-jj-repo
   (consult-jj-test-sh "jj commit -m first-commit")
   (let* ((log-buffer   (consult-jj--log))
          (candidate    (car (consult-jj--log-candidates log-buffer)))
          (expected     (consult-jj--revision-change-id
                         (overlay-get (cdr candidate) 'consult-jj--revision))))
     (kill-buffer log-buffer)
     (cl-letf (((default-value 'completing-read-function)
                (lambda (&rest _) (car candidate))))
       (should (string-equal (consult-jj-read-revision "jj diff at" "@-")
                             expected)))))
  ;; when the user enters custom text that matches no candidate,
  ;; then the text is passed through unchanged
  (with-test-jj-repo
   (consult-jj-test-sh "jj commit -m first-commit")
    (cl-letf (((default-value 'completing-read-function)
               (lambda (&rest _) "custom revset")))
     (should (string-equal (consult-jj-read-revision "jj diff at" "@-")
                           "custom revset")))))

(ert-deftest consult-jj-diff-at ()
  ;; when the revision adds two files, then consult-jj-diff-at shows both diffs
  (with-test-jj-repo
   (consult-jj-test-write-file "file1.txt" "file1.txt")
   (consult-jj-test-write-file "file2.txt" "file2.txt")
   (should (string-equal
            (consult-jj-test-buffer-string (consult-jj-diff-at "@"))
            "diff --git a/file1.txt b/file1.txt
new file mode 100644
index 0000000000..39cd5762dc\n--- /dev/null\n+++ b/file1.txt
@@ -0,0 +1,1 @@\n+file1.txt
\\ No newline at end of file
diff --git a/file2.txt b/file2.txt
new file mode 100644\nindex 0000000000..c3ee11c8b3
--- /dev/null
+++ b/file2.txt
@@ -0,0 +1,1 @@
+file2.txt
\\ No newline at end of file
")))
  ;; when the revision is empty, then consult-jj-diff-at returns an empty buffer
  (with-test-jj-repo
   (should (string-equal
            (consult-jj-test-buffer-string (consult-jj-diff-at "@"))
            "")))
  ;; when the commit is two revisions back, then consult-jj-diff-at "@--" shows its diff
  (with-test-jj-repo
   (consult-jj-test-write-file "file1.txt" "file1.txt")
   (consult-jj-test-sh "jj new" "jj new")
   (should (string-equal
            (consult-jj-test-buffer-string (consult-jj-diff-at "@--"))
            "diff --git a/file1.txt b/file1.txt
new file mode 100644
index 0000000000..39cd5762dc
--- /dev/null
+++ b/file1.txt
@@ -0,0 +1,1 @@
+file1.txt
\\ No newline at end of file
"))))

(ert-deftest consult-jj-diff-from ()
  ;; when files changed since, then consult-jj-diff-from shows both diffs
  (with-test-jj-repo
   (consult-jj-test-write-file "file1.txt" "one\n")
   (consult-jj-test-sh "jj new")
   (consult-jj-test-write-file "file1.txt" "two\n")
   (consult-jj-test-write-file "newfile.txt" "newfile\n")
   (should (string-equal
            (consult-jj-test-buffer-string (consult-jj-diff-from "@-"))
            "diff --git a/file1.txt b/file1.txt
index 5626abf0f7..f719efd430 100644
--- a/file1.txt
+++ b/file1.txt
@@ -1,1 +1,1 @@
-one
+two
diff --git a/newfile.txt b/newfile.txt
new file mode 100644
index 0000000000..aa39060d7e
--- /dev/null
+++ b/newfile.txt
@@ -0,0 +1,1 @@
+newfile
")))
  ;; when nothing changed, then consult-jj-diff-from returns an empty buffer
  (with-test-jj-repo
   (consult-jj-test-write-file "file1.txt" "one\n")
   (consult-jj-test-sh "jj new")
   (should (string-equal
            (consult-jj-test-buffer-string (consult-jj-diff-from "@-"))
            ""))))

(ert-deftest consult-jj-new ()
  ;; jj new creates a change on top of the revision and shows the result
  (with-test-jj-repo
   (consult-jj-test-sh "jj commit -m first-commit")
   (let ((parent-change-id (consult-jj--rev-change-id "@")))
     (cl-letf (((symbol-function 'display-message-or-buffer)
                #'consult-jj-test-display-message-or-buffer))
       (setq consult-jj-test-message nil)
       (consult-jj-new "@")
       (should (string-equal (consult-jj--rev-change-id "@-")
                             parent-change-id))
       (should (string-match-p "Working copy"
                               consult-jj-test-message)))))

  ;; when the command fails, then a jj-error is signaled
  (with-test-jj-repo
   (cl-letf (((symbol-function 'display-message-or-buffer)
              #'consult-jj-test-display-message-or-buffer))
     (should-error (consult-jj-new "nonexistent-rev")
                   :type 'jj-error))))

(ert-deftest consult-jj-edit ()
  ;; jj edit moves the working copy to the revision and shows the result
  (with-test-jj-repo
   (consult-jj-test-sh "jj commit -m first-commit")
   (let ((target-change-id (consult-jj--rev-change-id "@-")))
     (cl-letf (((symbol-function 'display-message-or-buffer)
                #'consult-jj-test-display-message-or-buffer))
       (setq consult-jj-test-message nil)
       (consult-jj-edit "@-")
       (should (string-equal (consult-jj--rev-change-id "@")
                             target-change-id))
       (should (string-match-p "Working copy"
                               consult-jj-test-message)))))

  ;; when the command fails, then a jj-error is signaled
  (with-test-jj-repo
   (consult-jj-test-sh "jj commit -m first-commit")
   (cl-letf (((symbol-function 'display-message-or-buffer)
              #'consult-jj-test-display-message-or-buffer))
     (should-error (consult-jj-edit "nonexistent-rev")
                   :type 'jj-error))))

(ert-deftest consult-jj-describe-mode-keybindings ()
  ;; the mode is derived from markdown-mode
  (should (eq (get 'consult-jj-describe-mode 'derived-mode-parent)
              'markdown-mode))
  ;; C-c C-c accepts the description
  (should (eq (lookup-key consult-jj-describe-mode-map (kbd "C-c C-c"))
              #'consult-jj-describe-accept))
  ;; C-c C-k rejects the description
  (should (eq (lookup-key consult-jj-describe-mode-map (kbd "C-c C-k"))
              #'consult-jj-describe-reject)))

(ert-deftest consult-jj-describe ()
  ;; when a revision has a description, then the new buffer shows it in
  ;; consult-jj-describe-mode with the change id stored as a buffer-local variable
  (with-test-jj-repo
   (consult-jj-test-sh "jj describe -m first-commit")
   (let* ((change-id (consult-jj--rev-change-id "@"))
          (buffer    (consult-jj-describe change-id)))
     (unwind-protect
         (progn
           (should (string-match-p "^first-commit\n\nJJ: Change ID: "
                                   (consult-jj-test-buffer-string buffer)))
           (should (eq (buffer-local-value 'major-mode buffer)
                       'consult-jj-describe-mode))
           (should (string-equal
                    (buffer-local-value 'consult-jj-describe-revision buffer)
                    change-id)))
       (kill-buffer buffer))))

  ;; when a revision has no description, then the buffer only contains the
  ;; comment block
  (with-test-jj-repo
   (let ((buffer (consult-jj-describe (consult-jj--rev-change-id "@"))))
     (unwind-protect
         (should (string-match-p "^JJ: Change ID: "
                                 (consult-jj-test-buffer-string buffer)))
       (kill-buffer buffer))))

  ;; when invoked without a revision, then the selected revision is described
  (with-test-jj-repo
   (consult-jj-test-sh "jj describe -m first-commit")
   (let* ((log-buffer (consult-jj--log))
          (candidate  (car (consult-jj--log-candidates log-buffer)))
          (expected   (consult-jj--revision-change-id
                       (overlay-get (cdr candidate) 'consult-jj--revision))))
     (kill-buffer log-buffer)
     (cl-letf (((default-value 'completing-read-function)
                (lambda (&rest _) (car candidate))))
       (let ((buffer (consult-jj-describe)))
         (unwind-protect
             (progn
               (should (string-equal
                        (buffer-local-value 'consult-jj-describe-revision buffer)
                        expected))
               (should (string-match-p "first-commit"
                                       (consult-jj-test-buffer-string buffer))))
           (kill-buffer buffer)))))))

(ert-deftest consult-jj-describe-accept ()
  ;; when the description is edited and accepted, then the description is set
  ;; on the change and the buffer is killed
  (with-test-jj-repo
   (consult-jj-test-sh "jj describe -m old-description")
   (let ((repo-dir default-directory))
     (let* ((change-id (consult-jj--rev-change-id "@"))
            (buffer    (consult-jj-describe change-id)))
       (with-current-buffer buffer
         (erase-buffer)
         (insert "new-description")
         (cl-letf (((symbol-function 'display-message-or-buffer)
                    #'consult-jj-test-display-message-or-buffer))
           (setq consult-jj-test-message nil)
           (consult-jj-describe-accept)))
       (should (string-equal consult-jj-test-message
                             "Description updated"))
       (should-not (buffer-live-p buffer))
       (should (string-equal (consult-jj-test-description change-id repo-dir)
                             "new-description\n")))))

  ;; when the revision does not exist, then a jj-error is signaled and the
  ;; buffer is left alive
  (with-test-jj-repo
   (consult-jj-test-sh "jj describe -m old-description")
   (let ((buffer (consult-jj-describe "@")))
     (with-current-buffer buffer
       (setq-local consult-jj-describe-revision "nonexistent-rev")
       (should-error (consult-jj-describe-accept)
                     :type 'jj-error)
       (should (buffer-live-p (current-buffer))))
     (kill-buffer buffer))))

(ert-deftest consult-jj-describe-accept-strips-comments ()
  ;; when the buffer contains the template's "JJ:" comment lines, then they are
  ;; stripped before the description is set
  (with-test-jj-repo
   (consult-jj-test-sh "jj describe -m old-description")
   (let ((repo-dir default-directory))
     (let* ((change-id (consult-jj--rev-change-id "@"))
            (buffer    (consult-jj-describe change-id)))
       (with-current-buffer buffer
         (cl-letf (((symbol-function 'display-message-or-buffer)
                    #'consult-jj-test-display-message-or-buffer))
           (setq consult-jj-test-message nil)
           (consult-jj-describe-accept)))
       (should (string-equal (consult-jj-test-description change-id repo-dir)
                             "old-description\n"))))))

(ert-deftest consult-jj-describe-reject ()
  ;; when the description is edited and rejected, then the buffer is killed
  ;; and the description is left unchanged
  (with-test-jj-repo
   (consult-jj-test-sh "jj describe -m original")
   (let ((repo-dir default-directory))
     (let* ((change-id (consult-jj--rev-change-id "@"))
            (buffer    (consult-jj-describe change-id)))
       (with-current-buffer buffer
         (erase-buffer)
         (insert "not saved")
         (consult-jj-describe-reject))
       (should-not (buffer-live-p buffer))
       (should (string-equal (consult-jj-test-description change-id repo-dir)
                             "original\n"))))))
