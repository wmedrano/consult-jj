;;; consult-jj.el --- JJ integration for consult  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30"))

;; Author: Will Medrano <wmedrano@wmedrano.dev>

;;; Commentary:
;;
;; This package provides `consult' integration for `jj'.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'ansi-color)

(defgroup consult-jj nil
  "JJ integration for consult."
  :group 'tools)

(defcustom consult-jj-executable "jj"
  "Path to the jj executable."
  :type 'string
  :group 'consult-jj)

(defcustom consult-jj-log-preview t
  "If querying for a revision should show a `jj log' preview buffer."
  :type 'boolean
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
  (declare (indent 1))
  `(let ((root              (consult-jj-root))
         (inhibit-read-only t)
         (buffer            (get-buffer-create ,name)))
     (with-current-buffer buffer
       (setq-local default-directory root)
       (erase-buffer)
       (delete-all-overlays)
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
(defun consult-jj--read-revision (prompt-prefix default-revision)
  "Read a revision from the user.

PROMPT-PREFIX is prepended to the prompt.
DEFAULT-REVISION is offered as the default."
  (let* ((active        t)
         (jj-log-window nil)
         (show-log      (lambda (buffer)
                          (when active
                            (setq jj-log-window
                                  (display-buffer-in-side-window
                                   buffer
                                   '((side              . bottom)
                                     (window-height     . fit-window-to-buffer)
                                     (window-parameters . ((mode-line-format . none))))))))))
    (if consult-jj-log-preview
        (consult-jj--log :on-done show-log))
    (unwind-protect
        (let* ((candidates nil)
               (rev        (thread-first  "%s revision (default %s): "
                                          (format prompt-prefix default-revision)
                                          (completing-read candidates nil nil)
                                          string-trim)))
          (if (string-empty-p rev)
              default-revision rev))
      (setq active nil)
      (when (and jj-log-window (window-live-p jj-log-window))
        (delete-window jj-log-window)))))

(cl-defun consult-jj--log (&key on-done)
  "Show the `jj log' output in a \"*jj-log*\" buffer.

The log is generated asynchronously; If ON-DONE is non-nil, it is called with
the log buffer after the log has been generated."
  (interactive)
  (with-consult-jj-buffer "*jj-log*"
    (consult-jj--start-process
     '("log")
     :on-done (lambda ()
                (consult-jj--log-finalize)
                (when on-done
                  (funcall on-done (current-buffer)))))))

(defun consult-jj--log-finalize ()
  "Prepare the *jj-log* buffer for display."
  (let ((inhibit-read-only t))
    (goto-char (point-min)))
  (read-only-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let* ((rev    (or rev (consult-jj--read-revision "jj diff at" "@")))
         (buffer (with-consult-jj-buffer "*jj-diff*"
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
  (let* ((from-rev (or from-rev (consult-jj--read-revision "jj diff from" "@-")))
         (buffer   (with-consult-jj-buffer "*jj-diff*"
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

(provide 'consult-jj)
;;; consult-jj.el ends here
