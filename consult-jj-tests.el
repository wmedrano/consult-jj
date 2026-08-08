;; -*- lexical-binding: t -*-

(require 'ert)
(require 'consult-jj)


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
    (list "jj" "commit" "-m" "initial-commit")
    (list "jj" "commit" "-m" "second-commit")
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
