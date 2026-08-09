;;; consult-jj.el --- JJ integration for consult  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30") (markdown-mode "2.0"))

;; Author: Will Medrano <wmedrano@wmedrano.dev>

;;; Commentary:
;;
;; This package provides `consult' integration for `jj'.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'ansi-color)
(require 'markdown-mode)

(defgroup consult-jj nil
  "JJ integration for consult."
  :group 'tools)

(defcustom consult-jj-executable "jj"
  "Path to the jj executable."
  :type 'string
  :group 'consult-jj)

(defface consult-jj-bookmark-face
  '((t :inherit font-lock-type-face))
  "Face for bookmarks in the jj log."
  :group 'consult-jj)

(defface consult-jj-change-id-face
  '((t :inherit font-lock-constant-face))
  "Face for change ids in the jj log."
  :group 'consult-jj)

(defface consult-jj-selected-face
  '((t :inherit highlight))
  "Face for the currently selected revision in the jj log."
  :group 'consult-jj)

(defface consult-jj-graph-face
  '((t :inherit font-lock-keyword-face))
  "Face for graph line characters in the jj log."
  :group 'consult-jj)

(defface consult-jj-email-face
  '((t :inherit font-lock-string-face))
  "Face for email addresses in the jj log."
  :group 'consult-jj)

(defface consult-jj-timestamp-face
  '((t :inherit font-lock-type-face))
  "Face for timestamps in the jj log."
  :group 'consult-jj)

(defface consult-jj-commit-id-face
  '((t :inherit font-lock-comment-face))
  "Face for commit ids in the jj log."
  :group 'consult-jj)

(defface consult-jj-no-description-face
  '((t :inherit font-lock-comment-face))
  "Face for \"(no description set)\" in the jj log."
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

(defmacro consult-jj--with-reused-buffer (name &rest body)
  "Prepare the buffer NAME and execute BODY in it.

The buffer is created if needed.  Otherwise, the buffer is reused, erased, and
its `default-directory' is set to the root of the current jj repository.
Returns the buffer."
  (declare (indent 1))
  `(let ((root              (consult-jj-root))
         (buffer            (get-buffer-create ,name)))
     (with-current-buffer buffer
       (setq-local default-directory root)
       (read-only-mode -1)
       (erase-buffer)
       (delete-all-overlays)
       ,@body
       buffer)))

(defmacro consult-jj--with-new-buffer (name &rest body)
  "Create a new buffer named NAME and execute BODY in it.

The buffer is created with `generate-new-buffer', so a unique name is used
if NAME is already taken.  Its `default-directory' is set to the root of the
current jj repository.  Returns the buffer."
  (declare (indent 1))
  `(let ((default-directory (consult-jj-root))
         (buffer            (generate-new-buffer ,name)))
     (with-current-buffer buffer
       ,@body
       buffer)))

(cl-defun consult-jj--start-process (args &key on-done)
  "Start a `jj' process with ARGS.

COLOR is passed to the `--color' flag; it defaults to \"never\".
ON-DONE is called in the process buffer when the process exits."
  (let* ((command    (car args))
         (final-args (append '("--color" "never" "--no-pager")
                             args))
         (sentinel   (if on-done (lambda (proc _event)
                                   (with-current-buffer (process-buffer proc)
                                     (funcall on-done)))))
         (proc (apply #'start-process (format "jj-%s" command)
                      (current-buffer)
                      consult-jj-executable
                      final-args)))
    (if sentinel (set-process-sentinel proc sentinel))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revisions / Log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vertico-sort-function)

(declare-function vertico--candidate "vertico")

(defun consult-jj-read-revision (prompt-prefix default-revision)
  "Read a revision from the user.

PROMPT-PREFIX is prepended to the prompt.
DEFAULT-REVISION is offered as the default."
  (let* ((jj-log-buffer (consult-jj--log))
         (jj-log-window (display-buffer-in-side-window
                         jj-log-buffer
                         '((side              . bottom)
                           (window-height     . fit-window-to-buffer)
                           (window-parameters . ((mode-line-format . none))))))
         (candidates    (consult-jj--log-candidates jj-log-buffer))
         ;; The candidates are already sorted by `jj log' output.
         (vertico-sort-function nil))
    (when (string= default-revision "@")
      (consult-jj--read-revision-highlight-candidate (caar candidates)
                                                     candidates))
    (unwind-protect
        (let ((rev (minibuffer-with-setup-hook
                       (lambda ()
                         (add-hook 'post-command-hook
                                   (lambda ()
                                     (when (fboundp 'vertico--candidate)
                                       (consult-jj--read-revision-highlight-candidate
                                        (vertico--candidate)
                                        candidates)))
                                   nil t))
                     (completing-read
                      (format "%s revision (default %s): " prompt-prefix default-revision)
                      candidates))))
          (cond
           ;; Empty -> Default
           ((string-empty-p rev) default-revision)
           ;; Revision -> Change ID
           ((assoc rev candidates)
            (consult-jj--revision-change-id
             (overlay-get (cdr (assoc rev candidates)) 'consult-jj--revision)))
           ;; Custom text
           (t rev)))
      (when (window-live-p jj-log-window)
        (delete-window jj-log-window))
      (when (buffer-live-p jj-log-buffer)
        (kill-buffer jj-log-buffer)))))

(defun consult-jj--read-revision-highlight-candidate (candidate candidates)
  "Highlight the log entry for the selected CANDIDATE.

CANDIDATES is the alist of candidate text to overlay.  When CANDIDATE
is not in CANDIDATES, the highlight is left unchanged."
  (when-let* ((overlay   (cdr (assoc candidate candidates))))
    (dolist (entry candidates)
      (overlay-put (cdr entry) 'face
                   (when (eq (cdr entry) overlay)
                     'consult-jj-selected-face)))))


(defconst consult-jj--revision-fields
  '((:change-id          . "json(change_id)")
    (:change-id-shortest . "json(change_id.shortest().prefix())")
    (:commit-id          . "json(commit_id)")
    (:description        . "json(description.first_line())")
    (:bookmarks          . "json(bookmarks.map(|b| b.name()))"))
  "Alist mapping revision field keywords to jj template expressions.")

(cl-defstruct (consult-jj--revision (:constructor consult-jj--make-revision))
  change-id
  change-id-shortest
  commit-id
  description
  bookmarks)

(define-derived-mode consult-jj--log-mode special-mode "jj-log"
  "Major mode for displaying `jj log' output."
  (setq-local font-lock-defaults
              '((("^\\([ @◆○×│~├─╮╯╰╭┤┬┴┼]+\\)" 1 'consult-jj-graph-face)
                 ("^[ @◆○×│~├─╮╯╰╭┤┬┴┼]+\\s-+\\([a-z]+\\)\\b" 1 'consult-jj-change-id-face)
                 ("\\b\\([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]\\{2,\\}\\)\\b" 1 'consult-jj-email-face)
                 ("\\b\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\b" 1 'consult-jj-timestamp-face)
                 ("\\b\\([0-9a-f]\\{8,\\}\\)$" 1 'consult-jj-commit-id-face)
                 ("\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\s-+\\(.+?\\)\\s-+[0-9a-f]\\{8,\\}$" 1 'consult-jj-bookmark-face)
                 ("(no description set)" 0 'consult-jj-no-description-face)))))

(defun consult-jj--log ()
  "Show the `jj log' output in a \"*jj-log*\" buffer.

The log is generated synchronously.  Returns the log buffer."
  (interactive)
  (consult-jj--with-new-buffer "*jj-log*"
    (let ((status (call-process consult-jj-executable nil t nil
                                "--color" "never" "--no-pager"
                                "log" "-T"
                                (string-join (append (mapcar #'cdr consult-jj--revision-fields) '("builtin_log_compact"))
                                             "++"))))
      (unless (zerop status)
        (signal 'jj-error (buffer-string)))
      (consult-jj--log-finalize))))

(defun consult-jj--log-finalize ()
  "Prepare the jj log buffer for display."
  (goto-char (point-min))
  (save-excursion
    (while (search-forward "\"" nil t)
      (backward-char)
      (let* ((start         (line-beginning-position))
             (json-start    (point))
             (revision-args (cl-loop for (field . _template) in consult-jj--revision-fields
                                     append (list field (json-parse-buffer))))
             (revision      (apply #'consult-jj--make-revision revision-args)))
        (delete-region json-start (point))
        (forward-line 2)
        (overlay-put (make-overlay start (point))
                     'consult-jj--revision
                     revision))))
  (consult-jj--log-mode))

(defun consult-jj--log-candidates (buffer)
  "Return an alist of candidate text to overlay for BUFFER.

Each element is a cons cell `(TEXT . OVERLAY)'.  TEXT is the revision's
change id (trimmed to 8 characters), description, and bookmarks."
  (with-current-buffer buffer
    (cl-loop for overlay in (overlays-in (point-min) (point-max))
             for revision = (overlay-get overlay 'consult-jj--revision)
             when revision
             collect (let* ((change-id       (propertize (substring (consult-jj--revision-change-id revision) 0 8)
                                                         'face 'consult-jj-change-id-face))
                            (raw-description (consult-jj--revision-description revision))
                            (description     (if (string-empty-p raw-description)
                                                 (propertize "(no description set)" 'face 'consult-jj-no-description-face)
                                               raw-description))
                            (bookmarks       (propertize (string-join (consult-jj--revision-bookmarks revision) " ")
                                                         'face 'consult-jj-bookmark-face))
                            (text            (string-trim
                                              (string-join
                                               (list change-id description bookmarks)
                                               " "))))
                       (cons text overlay)))))

(defun consult-jj--rev-change-id (rev)
  "Get the change id of REV."
  (with-temp-buffer
    (let ((status (call-process consult-jj-executable nil t nil
                                "--color" "never" "--no-pager"
                                "log" "--no-graph" "-r" rev
                                "-T" "json(change_id)")))
      (unless (zerop status)
        (error "JJ log failed: %s" (buffer-string)))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :array-type 'list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Describe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local consult-jj-describe-revision nil
  "Change id of the revision whose description is being edited.

Buffer-local in `consult-jj-describe-mode' buffers.")

(define-derived-mode consult-jj-describe-mode markdown-mode "jj-describe"
  "Major mode for editing a jj change description.

The revision being edited is recorded in the buffer-local variable
`consult-jj-describe-revision'.  `consult-jj-describe-accept' sets the new
description on the change and kills the buffer.  `consult-jj-describe-reject'
discards the edit and kills the buffer."
  (setq-local consult-jj-describe-revision nil))

(define-key consult-jj-describe-mode-map (kbd "C-c C-c") #'consult-jj-describe-accept)
(define-key consult-jj-describe-mode-map (kbd "C-c C-k") #'consult-jj-describe-reject)

;;;###autoload
(defun consult-jj-describe (&optional rev)
  "Edit the description of revision REV in a \"*jj-describe*\" buffer.

If REV is nil, prompt with `completing-read', defaulting to \"@\"."
  (interactive)
  (let* ((rev    (or rev (consult-jj-read-revision "jj describe" "@")))
         ;; TODO: If a jj-describe buffer already exists for `rev', we should
         ;; use that.
         (buffer (consult-jj--with-new-buffer "*jj-describe*"
                   (consult-jj-describe--start rev))))
    (pop-to-buffer buffer)
    buffer))

(defun consult-jj-describe--start (rev)
  "Dump the description of REV into the current buffer.

The description is followed by a comment block prefixed with
\"JJ: \", showing the change id.  `consult-jj-describe-accept'
removes those comment lines before sending the buffer to
`jj describe --stdin'.

Enables `consult-jj-describe-mode' and records REV in
`consult-jj-describe-revision'."
  (let* ((describe-template (string-join
                             '("description"
                               "\"\\n\""
                               "\"JJ: Change ID: \""
                               "change_id.shortest()"
                               "\"\\n\""
                               "\"JJ:\\n\""
                               "\"JJ: Lines starting with \\\"JJ:\\\" (like this one) will be removed.\\n\"")
                             " ++ "))
         (status            (call-process consult-jj-executable nil t nil
                                          "--color" "never" "--no-pager"
                                          "log"
                                          "-T" describe-template
                                          "--no-graph" "-r" rev)))
    (unless (zerop status)
      (signal 'jj-error (buffer-string))))
  (consult-jj-describe-mode)
  (setq-local
   consult-jj-describe-revision rev
   header-line-format (substitute-command-keys
                       (string-join
                        '("JJ Describe"
                          "Accept (\\[consult-jj-describe-accept])"
                          "Reject (\\[consult-jj-describe-reject])")
                        " | "))))

(defun consult-jj-describe-accept ()
  "Set the buffer's contents as the description of the revision.

Sends the buffer to `jj describe --stdin' for the revision in
`consult-jj-describe-revision', then kills the buffer."
  (interactive)
  (delete-trailing-whitespace)
  (flush-lines "^JJ:" (point-min) (point-max))
  (let ((end (point-max)))
    (goto-char end)
    (skip-chars-backward "\n")
    (unless (= (point) end)
      (delete-region (point) end)))
  (goto-char (point-min))
  (let ((start (point)))
    (skip-chars-forward "\n")
    (unless (= (point) start)
      (delete-region start (point))))
  (let ((status (call-process-region (point-min) (point-max)
                                     consult-jj-executable nil nil t
                                     "--color" "never" "--no-pager"
                                     "describe" "-r" consult-jj-describe-revision
                                     "--stdin")))
    (unless (zerop status)
      (signal 'jj-error (buffer-string))))
  (display-message-or-buffer "Description updated")
  (kill-buffer))

(defun consult-jj-describe-reject ()
  "Kill the buffer without saving the description."
  (interactive)
  (kill-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New/Edit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consult-jj--run-command (args)
  "Run `jj' with ARGS and display its output.

Runs synchronously and signals a `jj-error' on failure."
  (with-temp-buffer
    (let ((status (apply #'call-process consult-jj-executable nil t nil
                         (append '("--color" "never" "--no-pager") args))))
      (unless (zerop status)
        (signal 'jj-error (buffer-string)))
      (display-message-or-buffer (string-trim (buffer-string))))))

;;;###autoload
(defun consult-jj-new (&optional rev)
  "Create a new change on top of revision REV.

If REV is nil, then prompt with `completing-read', defaulting to \"@\".
Runs `jj new' synchronously and displays its output."
  (interactive)
  (let ((rev (or rev (consult-jj-read-revision "jj new" "@"))))
    (consult-jj--run-command `("new" ,rev))))

;;;###autoload
(defun consult-jj-edit (&optional rev)
  "Move the working copy to revision REV.

If REV is nil, then prompt with `completing-read', defaulting to \"@\".
Runs `jj edit' synchronously and displays its output."
  (interactive)
  (let ((rev (or rev (consult-jj-read-revision "jj edit" "@"))))
    (consult-jj--run-command `("edit" ,rev))))

(provide 'consult-jj)
;;; consult-jj.el ends here
