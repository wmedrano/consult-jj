;;; consult-jj.el --- JJ integration for consult  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30"))

;; Author: Will Medrano <wmedrano@wmedrano.dev>

;;; Commentary:
;;
;; This package provides `consult' integration for `jj'.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

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

(define-error 'jj-error "JJ error")

(defun consult-jj--signal (message)
  "Signal a `jj-error' with the failure MESSAGE from a `jj' command.

When MESSAGE indicates that the current directory is not in a jj
repository, signal with a clearer message."
  (let ((trimmed (string-trim (string-remove-prefix "Error: " message))))
    (signal 'jj-error
            (list (cond
                   ((string-empty-p trimmed)
                    "jj failed.")
                   ((string-match-p "\\(no jj repo\\|failed to find repository\\)" trimmed)
                    "There is no jj repository here.  Run jj from a directory inside a jj repository.")
                   (t trimmed))))))


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
        (consult-jj--signal result)))))

(defmacro consult-jj--with-new-buffer (name &rest body)
  "Create a new buffer named NAME and execute BODY in it.

The buffer is created with `generate-new-buffer', so a unique name is used
if NAME is already taken.  Its `default-directory' is set to the root of the
current jj repository.  Returns the buffer."
  (declare (indent 1))
  `(let* ((default-directory (consult-jj-root))
          (buffer            (generate-new-buffer ,name)))
     (with-current-buffer buffer
       ,@body
       buffer)))

(cl-defun consult-jj--start-process (args &key on-done)
  "Start a `jj' process with ARGS.

The process is started with `--color never' and `--no-pager'.
ON-DONE is called in the process buffer when the process exits."
  (let* ((command    (car args))
         (final-args (append '("--color" "never" "--no-pager")
                             args))
         (sentinel   (if on-done (lambda (proc _event)
                                   (when (and (eq (process-status proc) 'exit)
                                              (buffer-live-p (process-buffer proc)))
                                     (with-current-buffer (process-buffer proc)
                                       (funcall on-done))))))
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
        (consult-jj--signal (buffer-string)))
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
             collect (let* ((change-id       (let ((id (consult-jj--revision-change-id revision)))
                                               (propertize (substring id 0 (min 8 (length id)))
                                                           'face 'consult-jj-change-id-face)))
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
        (consult-jj--signal (buffer-string)))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :array-type 'list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New/Edit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar consult-jj--display-function #'display-message-or-buffer)

(defun consult-jj--run-command (args)
  "Run `jj' with ARGS and display its output.

Runs synchronously and signals a `jj-error' on failure."
  (with-temp-buffer
    (let ((status (apply #'call-process consult-jj-executable nil t nil
                         (append '("--color" "never" "--no-pager") args))))
      (unless (zerop status)
        (consult-jj--signal (buffer-string)))
      (funcall consult-jj--display-function (string-trim (buffer-string))))))

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
