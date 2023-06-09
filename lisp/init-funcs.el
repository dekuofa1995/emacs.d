;;; init-funcs.el -- Define functions. -*- lexical-binding: t -*-
;;; Commentary:
(require 'f)
(require 'cl-lib)
(require 'init-const)
(declare-function feature-file		"ext:loadhist.el")
(declare-function file-dependents	"ext:loadhist.el")
(declare-function org-babel-tangle-file "ext:ob.el")
;;; Code:
(defun deku/search-deps (feat)
  "Search who load the FEAT file."
  (require 'loadhist)
  (file-dependents (feature-file feat)))

;;;###autoload
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and deku/show-icon
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

;;;###autoload
(defun deku/config-refresh ()
  "Force refresh all my config files."
  (interactive)
  (deku/gen-all-org-init-files)
  (deku/load-all-init-files))

(defun deku/load-theme (&optional theme)
  (when-let (theme (or theme deku/theme))
    (load-theme theme)))

(provide 'init-funcs)
;;; init-funcs.el ends here
