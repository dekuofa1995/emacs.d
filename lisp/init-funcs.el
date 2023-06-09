;;; init-funcs.el -- Define functions. -*- lexical-binding: t -*-
;;; Commentary:
;; TODO: use borg(?) to load below packages
(dolist (name '("f" "s" "dash"))
  (add-to-list 'load-path (expand-file-name (format "lib/%s" name)
					    user-emacs-directory)))
(require 'f)
(require 'cl-lib)
(require 'init-const)
(declare-function feature-file		"ext:loadhist.el")
(declare-function file-dependents	"ext:loadhist.el")
(declare-function org-babel-tangle-file "ext:ob.el")
;;; Code:
;;;###autoload
(defun my/get-elisp-file-name (config-name)
  "Return the given CONFIG-NAME's elisp file  name."
  (format "%s.el" config-name))

;;;###autoload
(defun my/get-org-file-name (config-name)
  "Return the given CONFIG-NAME's org file name."
  (format "%s.org" config-name))

;;;###autoload
(defun my/config-backup (config-name target)
  "Backup config file by given CONFIG-NAME.
TARGET is source file path.
Store the backup file to `user-emacs-directory' + MY/CONFIG-BACKUP-DIR"
  (let* ((backup-dir (expand-file-name my/config-backup-dir user-emacs-directory))
	 (backup-file (expand-file-name (my/get-elisp-file-name config-name) backup-dir)))
    (when (file-exists-p target)
      (unless (file-exists-p backup-dir) (make-directory backup-dir))
      (copy-file target backup-file t))))

;;;###autoload
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
	(org-babel-tangle-file org-file target-file))
      ;;	(byte-compile-file target-file))
      (add-to-list 'load-path target-dir)
      (require (intern config-name)))))


(defun deku/search-deps (feat)
  "Search who load the FEAT file ."
  (require 'loadhist)
  (file-dependents (feature-file feat)))

;;;###autoload
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and my-show-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))


;;;###autoload
(defun deku/org-conf-file-name (&optional org-name)
  "Return .el file name with ORG-NAME or the name of current buffer."
  (let* ((f (or org-name (buffer-file-name) (buffer-name)))
         (fname (file-name-base f)))
    (format "init-%s" fname)))

;;;###autoload
(defun deku/gen-org-init-file (f-path out-path)
  "Generate Emacs init file from given org file with F-PATH store to OUT-PATH."
  (require 'org)
  (require 'ob)
  (org-babel-tangle-file f-path out-path))


;;;###autoload
(defun deku/file-extensionp (f-name extension)
  "Return Non-Nil when file with F-NAME is the EXTENSION file."
  (let ((ext (concat (if (string-match-p "^\\." extension)
			 extension
		       (concat "." extension))
		     "$")))
    (string-match-p ext f-name)))

;;;###autoload
(defun deku/f-files-with-extension (d-path extension &optional recursive)
  "Return the files have EXTENSION with the directory path D-PATH."
  (f-files d-path
	   (lambda (f) (deku/file-extensionp f extension))
	   recursive))

;;;###autoload
(defun deku/gen-all-org-init-files ()
  "Generate all of Emacs init file from Org files."
  (interactive)
  (when-let ((dirs deku/org-conf-dirs)
	     (org-path deku/org-conf-path)
	     (out-parent-dir deku/conf-path))
    (dolist (dir dirs)
      (let ((out-dir (file-name-concat out-parent-dir dir))
	    (org-dir (file-name-concat org-path dir)))
	(make-directory out-dir t)
	(dolist (f (deku/f-files-with-extension org-dir "org"))
	  (deku/gen-org-init-file
	   f
	   (file-name-concat out-dir
			     (concat (deku/org-conf-file-name (file-name-base f))
				     ".el"))))))))

;;;###autoload
(defun deku/load-all-init-files (&optional dirs)
  "Loading all init Files.

If DIRS Nill will search with `deku/org-conf-dirs'.
If not, then search with DIRS."
  (interactive)
  (when-let ((conf-path deku/conf-path)
	     (dirs (or dirs deku/org-conf-dirs)))
    (dolist (dir dirs)
      (let ((conf-dir (file-name-concat conf-path dir)))
	(add-to-list 'load-path conf-dir)
	(message "Start Loading directoary: %s" dir)
	(dolist (f (deku/f-files-with-extension conf-dir "el"))
	  (message "Loading %s" (file-name-base f))
	  (require (intern (file-name-base f))))
	(message "End Loading directoary: %s" dir)))))

(provide 'init-funcs)
;;; init-funcs.el ends here
