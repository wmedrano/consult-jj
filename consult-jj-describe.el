;;; consult-jj-describe.el --- Edit jj change descriptions -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30") (markdown-mode "2.0"))

;; Author: Will Medrano <wmedrano@wmedrano.dev>

;;; Commentary:
;;
;; This file provides `consult-jj-describe', which edits jj change
;; descriptions in a `markdown-mode' buffer.

;;; Code:

(require 'consult-jj)
(require 'consult-jj-diff)
(require 'markdown-mode)

(defvar-local consult-jj--describe-revision nil
  "Change id of the revision whose description is being edited.

Buffer-local in `consult-jj-describe-mode' buffers.")
(put 'consult-jj--describe-revision 'permanent-local t)

(define-derived-mode consult-jj-describe-mode markdown-mode "jj-describe"
  "Major mode for editing a jj change description.

The revision being edited is recorded in the buffer-local variable
`consult-jj--describe-revision'.  `consult-jj-describe-accept' sets the new
description on the change and kills the buffer.  `consult-jj-describe-reject'
discards the edit and kills the buffer."
  (font-lock-add-keywords nil '(("^JJ:.*" . font-lock-comment-face))))

(define-key consult-jj-describe-mode-map (kbd "C-c C-c") #'consult-jj-describe-accept)
(define-key consult-jj-describe-mode-map (kbd "C-c C-k") #'consult-jj-describe-reject)
(define-key consult-jj-describe-mode-map (kbd "C-c C-d") #'consult-jj-describe-diff)

;;;###autoload
(defun consult-jj-describe (rev)
  "Edit the description of revision REV in a \"*jj-describe*\" buffer.

REV is the revision.  When called interactively, prompt with `completing-read',
defaulting to \"@\"."
  (interactive (list (consult-jj-read-revision "jj describe" "@")))
  (let* (;; TODO: If a jj-describe buffer already exists for `rev', we should
         ;; use that.
         (buffer (consult-jj--with-new-buffer "*jj-describe*"
                   (consult-jj--describe-start rev))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj--describe-start (rev)
  "Dump the description of REV into the current buffer.

The description is followed by a comment block prefixed with
\"JJ: \", showing the change id.  `consult-jj-describe-accept'
removes those comment lines before sending the buffer to
`jj describe --stdin'.

Enables `consult-jj-describe-mode' and records the change id parsed
from the buffer in `consult-jj--describe-revision'."
  (let* ((describe-template
          (string-join
           '("description"
             "\"\\n\\n\""
             "\"JJ: Change ID: \""
             "change_id"
             "\"\\n\""
             "\"JJ:\\n\""
             "\"JJ: Lines starting with \\\"JJ:\\\" (like this one) will be removed.\\n\"")
           " ++ "))
         (status
          (apply #'call-process consult-jj-executable nil t nil
                 (append consult-jj-global-args
                         `("log"
                           "-T" ,describe-template
                           "--no-graph" "-r" ,rev)))))
    (unless (zerop status)
      (consult-jj--signal (buffer-string))))
  (goto-char (point-min))
  (if (re-search-forward "^JJ: Change ID: \\([a-z0-9]+\\)" nil t)
      (setq-local consult-jj--describe-revision
                  (match-string-no-properties 1))
    (setq-local consult-jj--describe-revision rev))
  (goto-char (point-min))
  (consult-jj-describe-mode)
  (setq-local
   header-line-format (substitute-command-keys
                       (string-join
                        '("JJ Describe"
                          "Accept (\\[consult-jj-describe-accept])"
                          "Reject (\\[consult-jj-describe-reject])"
                          "Diff (\\[consult-jj-describe-diff])")
                        " | "))))

(defun consult-jj-describe-accept ()
  "Set the buffer's contents as the description of the revision.

- Kills the buffer on success.
- Lines starting with JJ: are trimmed.
- Trailing whitespace is also removed."
  (interactive)
  (unless (derived-mode-p 'consult-jj-describe-mode)
    (user-error "Not in a `consult-jj-describe-mode' buffer"))
  (unless consult-jj--describe-revision
    (user-error "No revision is being described in this buffer"))
  (let ((buffer         (current-buffer))
        (revision       consult-jj--describe-revision)
        (tmp-error-file (make-temp-file "consult-jj-describe-")))
    (unwind-protect
        (unless (zerop (with-temp-buffer
                         (consult-jj--insert-sanitized-describe buffer)
                         (apply #'call-process-region (point-min) (point-max)
                                consult-jj-executable nil
                                (list nil tmp-error-file)
                                nil
                                (append consult-jj-global-args
                                        `("describe" "-r" ,revision "--stdin")))))
          (consult-jj--signal (with-temp-buffer
                                (insert-file-contents tmp-error-file)
                                (buffer-string))))
      (delete-file tmp-error-file))
    (with-current-buffer buffer
      (funcall consult-jj--display-function "Description for %s updated" revision)
      (kill-buffer))))

(defun consult-jj--insert-sanitized-describe (src-buffer)
  "Copy the contents of SRC-BUFFER over to the current buffer and sanitize.

This involves removing lines that start with JJ: and cleaning up some
whitespace."
  (insert-buffer-substring src-buffer)
  (flush-lines "^JJ:" (point-min) (point-max))
  (delete-trailing-whitespace)
  (let ((end (point-max)))
    (goto-char end)
    (skip-chars-backward "\n")
    (unless (= (point) end)
      (delete-region (point) end)))
  (goto-char (point-min))
  (let ((start (point)))
    (skip-chars-forward "\n")
    (unless (= (point) start)
      (delete-region start (point)))))

(defun consult-jj-describe-reject ()
  "Kill the buffer without saving the description."
  (interactive)
  (unless (derived-mode-p 'consult-jj-describe-mode)
    (user-error "Not in a `consult-jj-describe-mode' buffer"))
  (unless consult-jj--describe-revision
    (user-error "No revision is being described in this buffer"))
  (kill-buffer))

(defun consult-jj-describe-diff ()
  "Show the diff of the change being described in the current buffer.

Displays the diff of `consult-jj--describe-revision' using
`consult-jj-diff-at', which pops to a new \"*jj-diff*\" buffer."
  (interactive)
  (unless (derived-mode-p 'consult-jj-describe-mode)
    (user-error "Not in a `consult-jj-describe-mode' buffer"))
  (unless consult-jj--describe-revision
    (user-error "No revision is being described in this buffer"))
  (consult-jj-diff-at consult-jj--describe-revision))

(provide 'consult-jj-describe)
;;; consult-jj-describe.el ends here
