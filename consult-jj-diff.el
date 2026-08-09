;;; consult-jj-diff.el --- Show jj diffs -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30") (consult-jj))

;; Author: Will Medrano <wmedrano@wmedrano.dev>

;;; Commentary:
;;
;; This file provides `consult-jj-diff', which shows diffs of jj
;; revisions in `diff-mode'.

;;; Code:

(require 'consult-jj)

;;;###autoload
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

;;;###autoload
(defun consult-jj-diff-at (&optional rev)
  "Show the diff of revision REV in a \"*jj-diff*\" buffer.

If REV is nil, then prompt with `completing-read', defaulting to \"@\".  The
diff is generated asynchronously and displayed with `diff-mode'."
  (interactive)
  (let* ((rev    (or rev (consult-jj-read-revision "jj diff at" "@")))
         (buffer (consult-jj--with-reused-buffer "*jj-diff*"
                   (consult-jj-diff-at--start rev))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj-diff-at--start (rev)
  "Start generating the diff at revision REV."
  (consult-jj--start-process `("diff" "--git" "-r" ,rev)
                             :on-done #'consult-jj-diff--finalize))

;;;###autoload
(defun consult-jj-diff-from (&optional from-rev)
  "Show the diff of the working copy from FROM-REV in a \"*jj-diff*\" buffer.

When called interactively, prompt for FROM-REV with `completing-read',
defaulting to \"@-\".  The diff is generated asynchronously and
displayed with `diff-mode'."
  (interactive)
  (let* ((from-rev (or from-rev (consult-jj-read-revision "jj diff from" "@-")))
         (buffer   (consult-jj--with-reused-buffer "*jj-diff*"
                     (consult-jj-diff-from--start from-rev))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj-diff-from--start (from-rev)
  "Start generating the diff from FROM-REV."
  (consult-jj--start-process `("diff" "--git" "--from" ,from-rev)
                             :on-done #'consult-jj-diff--finalize))

(defun consult-jj-diff--finalize ()
  "Finalize the diff buffer by enabling `diff-mode' and `read-only-mode'."
  (goto-char (point-min))
  (diff-mode)
  (read-only-mode 1))

(provide 'consult-jj-diff)
;;; consult-jj-diff.el ends here
