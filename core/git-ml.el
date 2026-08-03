;;; git-ml.el --- Repository-cached Git mode line  -*- lexical-binding: t; -*-

;;; Commentary:

;; Git status is cached per repository and shared by all visiting buffers.
;; Refreshes are debounced and each repository owns at most one Git process.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'git-ml-status)
(require 'git-ml-repository)

(defcustom git-ml-refresh-delay 0.15
  "Seconds to debounce refreshes for the same repository."
  :type 'number
  :group 'git-ml)

(defcustom git-ml-process-timeout 10
  "Seconds before terminating a stuck background Git process."
  :type 'number
  :group 'git-ml)

(defcustom git-ml-check-git-action-p t
  "Whether to inspect repository files for an operation in progress."
  :type 'boolean
  :group 'git-ml)

(defvar git-ml-debug nil "Whether to log `git-ml' diagnostic messages.")
(defvar git-ml-render-function #'git-ml-render
  "Function called with a `git-ml-result' to produce mode-line text.")
(defvar git-ml nil "Git information displayed in the mode line.")
(make-variable-buffer-local 'git-ml)

(defsubst git-ml--debug (format-string &rest args)
  "Log FORMAT-STRING with ARGS when `git-ml-debug' is non-nil."
  (when git-ml-debug
    (apply #'message (concat "git-ml: " format-string) args)))

(defvar-local git-ml--repo-key nil)
(defvar-local git-ml--active nil)

(defconst git-ml--status-command
  '("git" "--no-optional-locks" "status" "--porcelain=v2" "--branch" "-z"
    "--untracked-files=normal" "--ignore-submodules=dirty"))

(defun git-ml--current-repo ()
  "Return the repository associated with the current buffer."
  (or (and git-ml--repo-key (gethash git-ml--repo-key git-ml--repositories))
      (git-ml--repo-for-directory
       (if-let* ((file buffer-file-name))
           (file-name-directory file)
         default-directory))))

(defun git-ml--publish (repo)
  "Publish REPO's rendered status to its subscribed buffers."
  (let ((key (git-ml--repo-git-dir repo))
        live-buffers)
    (dolist (buffer (git-ml--repo-buffers repo))
      (when (and (buffer-live-p buffer)
                 (equal (buffer-local-value 'git-ml--repo-key buffer) key))
        (push buffer live-buffers)
        (with-current-buffer buffer
          (setq git-ml (git-ml--repo-rendered repo)))))
    (setf (git-ml--repo-buffers repo) live-buffers)
    (force-mode-line-update t)))

(defun git-ml--subscribe (repo)
  "Subscribe the current buffer to REPO status updates."
  (unless (eq repo (and git-ml--repo-key
                        (gethash git-ml--repo-key git-ml--repositories)))
    (git-ml--unsubscribe)
    (setq git-ml--repo-key (git-ml--repo-git-dir repo)))
  (cl-pushnew (current-buffer) (git-ml--repo-buffers repo))
  (setq git-ml (git-ml--repo-rendered repo)))

(defun git-ml--unsubscribe ()
  "Remove the current buffer from its repository subscription."
  (when-let* ((repo (and git-ml--repo-key
                         (gethash git-ml--repo-key git-ml--repositories))))
    (setf (git-ml--repo-buffers repo)
          (delq (current-buffer) (git-ml--repo-buffers repo))))
  (setq git-ml--repo-key nil
        git-ml nil))

(defun git-ml--request-refresh (repo &optional immediate)
  "Request a refresh for REPO, optionally without debounce delay."
  (if (process-live-p (git-ml--repo-process repo))
      (setf (git-ml--repo-pending repo) t)
    (git-ml--cancel-timer (git-ml--repo-refresh-timer repo))
    (setf (git-ml--repo-refresh-timer repo)
          (run-at-time (if immediate 0 git-ml-refresh-delay) nil
                       #'git-ml--start-refresh
                       (git-ml--repo-git-dir repo)))))

(defun git-ml--timeout (key process)
  "Terminate PROCESS if it is still serving repository KEY."
  (when-let* ((repo (gethash key git-ml--repositories)))
    (when (and (eq process (git-ml--repo-process repo))
               (process-live-p process))
      (process-put process 'git-ml-timed-out t)
      (delete-process process))))

(defun git-ml--status-sentinel (process _event)
  "Consume status from PROCESS and update its repository cache."
  (when (memq (process-status process) '(exit signal failed))
    (let* ((key (process-get process 'git-ml-repo-key))
           (repo (gethash key git-ml--repositories))
           (buffer (process-buffer process)))
      (when (and repo (eq process (git-ml--repo-process repo)))
        (git-ml--cancel-timer (git-ml--repo-timeout-timer repo))
        (setf (git-ml--repo-timeout-timer repo) nil
              (git-ml--repo-process repo) nil
              (git-ml--repo-process-buffer repo) nil)
        (if (and (eq (process-status process) 'exit)
                 (zerop (process-exit-status process))
                 (buffer-live-p buffer))
            (condition-case err
                (let ((result
                       (git-ml--parse-status
                        (with-current-buffer buffer (buffer-string)))))
                  (when git-ml-check-git-action-p
                    (setf (git-ml-result-action result)
                          (git-ml--git-action (git-ml--repo-git-dir repo))))
                  (setf (git-ml--repo-result repo) result
                        (git-ml--repo-rendered repo)
                        (funcall git-ml-render-function result))
                  (git-ml--publish repo))
              (error (git-ml--debug "parse failed for %s: %S" key err)))
          (git-ml--debug "status failed for %s%s" key
                         (if (process-get process 'git-ml-timed-out)
                             " (timeout)" "")))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (git-ml--repo-pending repo)
          (setf (git-ml--repo-pending repo) nil)
          (git-ml--request-refresh repo t))))))

(defun git-ml--start-refresh (key)
  "Start the pending status process for repository KEY."
  (when-let* ((repo (gethash key git-ml--repositories)))
    (setf (git-ml--repo-refresh-timer repo) nil)
    (if (process-live-p (git-ml--repo-process repo))
        (setf (git-ml--repo-pending repo) t)
      (let* ((default-directory (git-ml--repo-root repo))
             (process-environment (cons "GIT_OPTIONAL_LOCKS=0"
                                        process-environment))
             (buffer (generate-new-buffer " *git-ml-status*")))
        (condition-case err
            (let ((process
                   (make-process
                    :name (format "git-ml:%s"
                                  (file-name-nondirectory
                                   (directory-file-name default-directory)))
                    :buffer buffer
                    :command git-ml--status-command
                    :coding 'utf-8-unix
                    :connection-type 'pipe
                    :sentinel #'git-ml--status-sentinel
                    :noquery t)))
              (process-put process 'git-ml-repo-key key)
              (setf (git-ml--repo-process repo) process
                    (git-ml--repo-process-buffer repo) buffer
                    (git-ml--repo-timeout-timer repo)
                    (run-at-time git-ml-process-timeout nil
                                 #'git-ml--timeout key process)))
          (error
           (kill-buffer buffer)
           (git-ml--debug "could not start status for %s: %S" key err)))))))

(defun git-ml-refresh (&rest _ignored)
  "Refresh the current repository, sharing work between all its buffers."
  (interactive)
  (if-let* ((repo (git-ml--current-repo)))
      (progn
        (when git-ml--active (git-ml--subscribe repo))
        (git-ml--request-refresh repo))
    (when git-ml--active (git-ml--unsubscribe))))

(defun git-ml-activate ()
  "Subscribe the current buffer to repository status updates."
  (interactive)
  (setq git-ml--active t)
  (add-hook 'after-save-hook #'git-ml-refresh nil t)
  (add-hook 'kill-buffer-hook #'git-ml--unsubscribe nil t)
  (git-ml-refresh))

;; Migrate a running daemon that loaded the former buffer-scoped version.
(remove-hook 'window-selection-change-functions #'git-ml-refresh)
(remove-hook 'after-save-hook #'git-ml-refresh)

(with-eval-after-load 'magit
  (add-hook 'magit-post-refresh-hook #'git-ml-refresh))

(provide 'git-ml)
;;; git-ml.el ends here
