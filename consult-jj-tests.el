;; -*- lexical-binding: t -*-

(require 'ert)

(defmacro with-test-jj-repo (&rest body)
  `(let ((default-directory (make-temp-file "consult-jj-test" t)))
     (shell-command-to-string "jj git init")
     ,@body))

(ert-deftest consult-jj-root ()
  (let ((default-directory (make-temp-file "root-error" t)))
    (should-error (consult-jj-root)
                  :type 'jj-error))
  (with-test-jj-repo
   (should (string-equal (consult-jj-root)
                         default-directory))))
