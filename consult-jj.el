;; -*- lexical-binding: t -*-

;; Package-Requires: (emacs "30")

(defcustom consult-jj-executable "jj"
  "Path to the jj executable.")

(put 'jj-error 'error-conditions '(jj-error error))
(put 'jj-error 'error-message "JJ error")

(defun consult-jj-root ()
  "Get the root directory.

Returns an error if the `default-directory' is not in a jj repository."
  (with-temp-buffer
    (let* ((status (call-process consult-jj-executable nil t nil
                                 "root"))
           (result (string-trim (buffer-substring (point-min)
                                                  (point-max)))))
      (if (= status 0)
          result
        (signal 'jj-error result)))))
