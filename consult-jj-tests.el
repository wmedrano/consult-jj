;; -*- lexical-binding: t -*-

(require 'ert)
(require 'consult-jj)

(defmacro with-test-jj-repo (&rest body)
  `(let ((default-directory (make-temp-file "consult-jj-test" t)))
     (shell-command-to-string "jj git init")
     ,@body))

(defun consult-jj-test-commit (description files)
  "Create a commit with DESCRIPTION, writing FILES.
Each FILE is a filename or a (filename . contents) pair.
A bare filename is treated as (filename . filename)."
  (dolist (file files)
    (let ((name     (if (consp file) (car file) file))
          (contents (if (consp file) (cdr file) file)))
      (let ((dir (file-name-directory name)))
        (when dir
          (make-directory dir t)))
      (write-region contents nil name nil 'silent)))
  (shell-command-to-string
   (format "jj describe -m %s" (shell-quote-argument description))))

(defun consult-jj-test-buffer-string (buffer)
  "Get the string for `buffer'.

Waits for all processes in buffer to terminate before getting the string."
  (with-current-buffer buffer
    (when-let ((proc (get-buffer-process (current-buffer))))
      (while (process-live-p proc)
        (sleep-for 0.01)))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest consult-jj-root ()
  (let ((default-directory (make-temp-file "root-error" t)))
    (should-error (consult-jj-root)
                  :type 'jj-error))
  (with-test-jj-repo
   (should (string-equal (consult-jj-root)
                         default-directory))))

(ert-deftest consult-jj-diff-at ()
  (with-test-jj-repo
   (consult-jj-test-commit "Initial commit" '("file1.txt" "file2.txt"))
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
  (with-test-jj-repo
   (consult-jj-test-commit "Empty commit" '())
   (let ((diff-buffer (consult-jj-diff-at "@")))
     (should (string-equal (consult-jj-test-buffer-string diff-buffer)
                           "")))))

(ert-deftest consult-jj-diff-from ()
  (with-test-jj-repo
   (consult-jj-test-commit "Add file" '(("file1.txt" . "one\n")))
   (shell-command-to-string "jj new")
   (write-region "two\n" nil "file1.txt" nil 'silent)
   (write-region "newfile\n" nil "newfile.txt" nil 'silent)
   (let ((diff-buffer (consult-jj-diff-from "@-")))
     (should (string-equal (consult-jj-test-buffer-string diff-buffer)
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
"))))
  (with-test-jj-repo
   (consult-jj-test-commit "Add file" '(("file1.txt" . "one\n")))
   (shell-command-to-string "jj new")
   (let ((diff-buffer (consult-jj-diff-from "@-")))
     (should (string-equal (consult-jj-test-buffer-string diff-buffer)
                           "")))))
