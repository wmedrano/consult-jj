;; -*- lexical-binding: t -*-

;; Package-Requires: (emacs "30")

(defcustom consult-jj-executable "jj"
  "Path to the jj executable.")

(put 'jj-error 'error-conditions '(jj-error error))
(put 'jj-error 'error-message "JJ error")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro with-consult-jj-buffer (name &rest body)
  "Prepare the buffer NAME and execute BODY in it.

The buffer is created if needed. Otherwise, the buffer is reused, erased, and
its `default-directory' is set to the root of the current jj repository.
Returns the buffer."
  `(let ((root              (consult-jj-root))
         (inhibit-read-only t)
         (buffer            (get-buffer-create ,name)))
     (with-current-buffer buffer
       (setq-local default-directory root)
       (erase-buffer)
       (delete-all-overlays)
       ,@body
       buffer)))

(defun consult-jj--read-revision (prompt-prefix default-revision)
  (completing-read (format "%s revision (default %s):" prompt-prefix default-revision)
                   (list default-revision)
                   nil nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun consult-jj-diff (arg)
  "Show the diff of revision REV in a \"*jj-diff*\" buffer.

When called interactively with a prefix argument, behave as
`consult-jj-diff-from'; otherwise, behave as `consult-jj-diff-at'.
The diff is generated asynchronously and displayed with
`diff-mode'."
  (interactive "P")
  (if arg
      (funcall-interactively #'consult-jj-diff-from)
    (funcall-interactively #'consult-jj-diff-at)))


(defun consult-jj-diff-at (&optional rev)
  "Show the diff of revision REV in a \"*jj-diff*\" buffer.

If REV is nil, then prompt with `completing-read', defaulting to \"@\".  The
diff is generated asynchronously and displayed with `diff-mode'."
  (interactive)
  (let* ((rev    (or rev (consult-jj--read-revision "jj diff at" "@"))
         (buffer (with-consult-jj-buffer "*jj-diff*"
                                         (consult-jj-diff-at--start rev))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj-diff-at--start (rev)
  (let* ((sentinel (lambda (proc event)
                     (with-current-buffer (process-buffer proc)
                       (consult-jj-diff--finalize))))
         (proc     (start-process "jj-diff"
                                  (current-buffer)
                                  consult-jj-executable
                                  "diff"
                                  "--git"
                                  "--color" "never"
                                  "--no-pager"
                                  "-r" rev)))
    (set-process-sentinel proc sentinel)))

(defun consult-jj-diff-from (&optional from-rev)
  "Show the diff of the working copy from FROM-REV in a \"*jj-diff*\" buffer.

When called interactively, prompt for FROM-REV with `completing-read',
defaulting to \"@-\".  The diff is generated asynchronously and
displayed with `diff-mode'."
  (interactive)
  (let* ((from-rev (or from-rev (consult-jj--read-revision "jj diff from" "@-")))
         (buffer   (with-consult-jj-buffer "*jj-diff*"
                                           (consult-jj-diff-from--start from-rev))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj-diff-from--start (from-rev)
  (let* ((sentinel (lambda (proc event)
                     (with-current-buffer (process-buffer proc)
                       (consult-jj-diff--finalize))))
         (proc     (start-process "jj-diff"
                                  (current-buffer)
                                  consult-jj-executable
                                  "diff"
                                  "--git"
                                  "--color" "never"
                                  "--no-pager"
                                  "--from" from-rev)))
    (set-process-sentinel proc sentinel)))

(defun consult-jj-diff--finalize ()
  (goto-char (point-min))
  (diff-mode)
  (read-only-mode t))

(provide 'consult-jj)
