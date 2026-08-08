;; -*- lexical-binding: t -*-

(require 'ert)
(require 'consult-jj)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consult-jj-test--sh (&rest commands)
  "Run shell COMMAND and additional COMMANDS, asserting each succeeds.

Each command may be a string or a list of strings (which are concatenated).
Runs synchronously in `default-directory'; output is discarded."
  (dolist (cmd commands)
    (let ((cmd-str (if (stringp cmd) cmd (apply #'concat cmd))))
      (should (zerop (call-process shell-file-name nil nil nil
                                   shell-command-switch cmd-str))))))

(defmacro with-test-jj-repo (&rest body)
  `(let ((default-directory (make-temp-file "consult-jj-test" t)))
     (consult-jj-test--sh "jj git init")
     ,@body))


(defun consult-jj-test--write-file (filename contents)
  "Write CONTENTS to FILENAME, overwriting if it exists."
  (write-region contents nil filename nil 'silent))

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

(ert-deftest consult-jj-diff-at ()
  ;; when the revision adds two files, then consult-jj-diff-at shows both diffs
  (with-test-jj-repo
   (consult-jj-test--write-file "file1.txt" "file1.txt")
   (consult-jj-test--write-file "file2.txt" "file2.txt")
   (let ((diff-buffer (consult-jj-diff-at "@")))
     (should (string-equal (consult-jj-test-buffer-string diff-buffer)
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
"))))
  ;; when the revision is empty, then consult-jj-diff-at returns an empty buffer
  (with-test-jj-repo
   (should (string-equal (consult-jj-test-buffer-string (consult-jj-diff-at "@"))
                         "")))
  ;; when the commit is two revisions back, then consult-jj-diff-at "@--" shows its diff
  (with-test-jj-repo
   (consult-jj-test--write-file "file1.txt" "file1.txt")
   (consult-jj-test--sh "jj new" "jj new")
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
   (consult-jj-test--write-file "file1.txt" "one\n")
   (consult-jj-test--sh "jj new")
   (consult-jj-test--write-file "file1.txt" "two\n")
   (consult-jj-test--write-file "newfile.txt" "newfile\n")
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
   (consult-jj-test--write-file "file1.txt" "one\n")
   (consult-jj-test--sh (list "jj describe -m " (shell-quote-argument "Add file"))
                         "jj new")
   (should (string-equal
            (consult-jj-test-buffer-string (consult-jj-diff-from "@-"))
            ""))))
