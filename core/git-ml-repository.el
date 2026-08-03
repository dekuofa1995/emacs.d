;;; git-ml-repository.el --- Repository identity for git-ml  -*- lexical-binding: t; -*-

;;; Commentary:

;; Locate Git worktrees without spawning Git and own the repository cache.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (git-ml--repo (:constructor git-ml--repo-create) (:copier nil))
  root git-dir result rendered buffers process process-buffer
  refresh-timer timeout-timer pending)

(defvar git-ml--repositories (make-hash-table :test #'equal)
  "Repository cache keyed by canonical Git directory.")

(defun git-ml--repository-paths (directory)
  "Return canonical (ROOT GIT-DIR) containing DIRECTORY, or nil."
  (unless (file-remote-p directory)
    (when-let* ((root (locate-dominating-file directory ".git"))
                (marker (expand-file-name ".git" root))
                (git-dir
                 (cond
                  ((file-directory-p marker) marker)
                  ((file-regular-p marker)
                   (with-temp-buffer
                     (insert-file-contents marker nil 0 4096)
                     (when (re-search-forward "\\`gitdir: \\(.+\\)$" nil t)
                       (expand-file-name (string-trim (match-string 1)) root)))))))
      (when (file-directory-p git-dir)
        (list (file-name-as-directory (file-truename root))
              (file-name-as-directory (file-truename git-dir)))))))

(defun git-ml--repo-for-directory (directory)
  "Return the cached repository containing DIRECTORY, creating it if needed."
  (when-let* ((paths (git-ml--repository-paths directory)))
    (pcase-let ((`(,root ,git-dir) paths))
      (or (gethash git-dir git-ml--repositories)
          (puthash git-dir
                   (git-ml--repo-create :root root :git-dir git-dir)
                   git-ml--repositories)))))

(defun git-ml--cancel-timer (timer)
  "Cancel TIMER when it is active."
  (when (timerp timer)
    (cancel-timer timer)))

(defun git-ml-clear-cache ()
  "Stop background work and clear every repository cache entry."
  (interactive)
  (maphash
   (lambda (_key repo)
     (git-ml--cancel-timer (git-ml--repo-refresh-timer repo))
     (git-ml--cancel-timer (git-ml--repo-timeout-timer repo))
     (when (process-live-p (git-ml--repo-process repo))
       (delete-process (git-ml--repo-process repo)))
     (when (buffer-live-p (git-ml--repo-process-buffer repo))
       (kill-buffer (git-ml--repo-process-buffer repo))))
   git-ml--repositories)
  (clrhash git-ml--repositories))

(provide 'git-ml-repository)
;;; git-ml-repository.el ends here
