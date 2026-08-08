;;; consult-jj.el --- JJ integration for consult  -*- lexical-binding: t -*-

;; Package-Requires: (emacs "30")

;; Author: Will Medrano <wmedrano@wmedrano.dev>

;;; Commentary:
;;
;; This package provides `consult' integration for `jj'.

;;; Code:

(defgroup consult-jj nil
  "JJ integration for consult."
  :group 'tools)

(defcustom consult-jj-executable "jj"
  "Path to the jj executable."
  :type 'string
  :group 'consult-jj)

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

The buffer is created if needed.  Otherwise, the buffer is reused, erased, and
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
  "Read a revision from the user.

PROMPT-PREFIX is prepended to the prompt.
DEFAULT-REVISION is offered as the default."
  (completing-read (format "%s revision (default %s):" prompt-prefix default-revision)
                   (list default-revision)
                   nil nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun consult-jj-diff (arg)
  "Show the diff of revision REV in a \"*jj-diff*\" buffer.

When called interactively with a prefix ARG, behave as
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
  (let* ((rev    (or rev (consult-jj--read-revision "jj diff at" "@")))
         (buffer (with-consult-jj-buffer "*jj-diff*"
                                         (consult-jj-diff-at--start rev))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj-diff-at--start (rev)
  "Start generating the diff at revision REV."
  (let* ((sentinel (lambda (proc _event)
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
  "Start generating the diff from FROM-REV."
  (let* ((sentinel (lambda (proc _event)
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
  "Finalize the diff buffer by enabling `diff-mode' and `read-only-mode'."
  (goto-char (point-min))
  (diff-mode)
  (read-only-mode t))

(provide 'consult-jj)
;;; consult-jj.el ends here
