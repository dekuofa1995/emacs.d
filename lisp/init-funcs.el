;;; init-funcs.el -- Define functions. -*- lexical-binding: t -*-
;;; Commentary:

(require 'init-const)
;;; Code:
(defun my/get-elisp-file-name (config-name)
  "Return the given CONFIG-NAME's elisp file  name."
  (format "%s.el" config-name))
(defun my/get-org-file-name (config-name)
  "Return the given CONFIG-NAME's org file name."
  (format "%s.org" config-name))

(defun my/config-backup (config-name target)
  "Backup config file by given CONFIG-NAME.
TARGET is source file path.
Store the backup file to `user-emacs-directory' + MY/CONFIG-BACKUP-DIR"
  (let* ((backup-dir (expand-file-name my/config-backup-dir user-emacs-directory))
	 (backup-file (expand-file-name (my/get-elisp-file-name config-name) backup-dir)))
    (when (file-exists-p target)
      (unless (file-exists-p backup-dir) (make-directory backup-dir))
      (copy-file target backup-file t))))

(defun my/config-load (config-name &optional force)
  "Load my custom configuration file by given CONFIG-NAME.
If FORCE NON-NIL then generate file wheather the old file exist or not
First try backup file, then generate new elisp file"
  (let* ((org-file(expand-file-name (my/get-org-file-name config-name) user-emacs-directory))
	 (target-dir (expand-file-name my/config-dir user-emacs-directory))
	 (target-file (expand-file-name (my/get-elisp-file-name config-name) target-dir)))
    (when (file-exists-p org-file)
      (make-directory target-dir t)
      (when (or force (not (file-exists-p target-file)))
	(require 'org)
	(require 'ob)
	(my/config-backup config-name target-file)
	(org-babel-tangle-file org-file target-file)
	(byte-compile-file target-file))
      ;; (require (intern config-name)))))
      (load target-file))))

(defun my/search-deps (feat)
  "Search who load the FEAT file ."
  (require 'loadhist)
  (file-dependents (feature-file feat)))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and my-show-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))
(provide 'init-funcs)
;;; init-funcs.el ends here
