;;; consult-jj-diff.el --- Show jj diffs -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30"))

;; Author: Will Medrano <wmedrano@wmedrano.dev>

;;; Commentary:
;;
;; This file provides commands for showing jj revision diffs in `diff-mode'.

;;; Code:

(require 'consult-jj)

;;;###autoload
(defun consult-jj-diff-at (rev)
  "Show the diff of revision REV in a new \"*jj-diff*\" buffer.

REV is the revision.  When called interactively, prompt with `completing-read',
defaulting to \"@\".  The diff is generated asynchronously and displayed with
`diff-mode'."
  (interactive (list (consult-jj-read-revision "jj diff at" "@")))
  (let* ((buffer (consult-jj--with-new-buffer "*jj-diff*"
                   (consult-jj--diff-run `("-r" ,rev)))))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun consult-jj-diff-from (from-rev &optional to-rev)
  "Show the diff from FROM-REV to TO-REV in a new \"*jj-diff*\" buffer.

FROM-REV is the starting revision.  When called interactively, prompt for
FROM-REV with `completing-read', defaulting to \"@-\".  With a prefix argument,
also prompt for TO-REV, defaulting to \"@\", and show the diff between the two
revisions; otherwise show the diff from FROM-REV to the working copy.  The diff
is displayed with `diff-mode'."
  (interactive
   (list (consult-jj-read-revision "jj diff from" "@-")
         (when current-prefix-arg
           (consult-jj-read-revision "jj diff to" "@"))))
  (let* ((buffer (consult-jj--with-new-buffer "*jj-diff*"
                   (consult-jj--diff-run
                    (if to-rev
                        `("--from" ,from-rev "--to" ,to-rev)
                      `("--from" ,from-rev))))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj--diff-run (args)
  "Run jj diff on the current buffer with ARGS."
  (consult-jj--start-process
   (append '("diff" "--git")
           args)
   :on-done #'consult-jj--diff-finalize))

(defun consult-jj--diff-finalize ()
  "Finalize the diff buffer by enabling `diff-mode' and `read-only-mode'."
  (goto-char (point-min))
  (diff-mode)
  (read-only-mode 1))

(provide 'consult-jj-diff)
;;; consult-jj-diff.el ends here
