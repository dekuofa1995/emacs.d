;;; init-funcs.el -- Define functions. -*- lexical-binding: t -*-
;;; Commentary:
(eval-when-compile
  (dolist (path '("lib/f" "lib/s" "lib/dash" "lib/setup"))
    (add-to-list 'load-path (expand-file-name path user-emacs-directory))))

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

;;;; Loading packages incrementally.
(defvar elemacs-incremental-packages '()
  "A list of packages to load incrementally after startup. Any large packages
  here may cause noticeable pauses, so it's recommended you break them up into
  sub-packages. For example, `org' is comprised of many packages, and can be
  broken up into:

    (elemacs-load-packages-incrementally
     \='(calendar find-func format-spec org-macs org-compat
       org-faces org-entities org-list org-pcomplete org-src
       org-footnote org-macro ob org org-clock org-agenda
       org-capture))

  Incremental loading does not occur in daemon sessions (they are
  loaded immediately at startup).")

(defcustom elemacs-incremental-first-idle-timer (if (daemonp) 0 1.5)
  "How long (in idle seconds) until incremental loading starts.

 Set this to nil to disable incremental loading. Set this to 0 to
load all incrementally deferred packages immediately at
`emacs-startup-hook'."
  :group 'elemacs-iloader
  :type 'number)

(defcustom elemacs-incremental-idle-timer 1.5
  "How long (in idle seconds) in between incrementally loading packages."
  :group 'elemacs-iloader
  :type 'number)

;;;; from https://github.com/Elilif/.elemacs/blob/68f6b338e8407af23fcb71fd33e6febaa226a15e/core/core-incremental-loading.el#L71
(defun elemacs-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally,
in `elemacs-incremental-idle-timer' intervals."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        (cl-callf append elemacs-incremental-packages packages)
      (while packages
        (let ((req (pop elemacs-incremental-packages)))
          (condition-case-unless-debug e
              (or (not
                   (while-no-input
                     ;; (message "Loading %s (%d left)" req (length elemacs-incremental-packages))
                     ;; If `default-directory' doesn't exist or is
                     ;; unreadable, Emacs throws file errors.
                     (let ((default-directory user-emacs-directory)
                           (inhibit-message t)
                           (file-name-handler-alist
                            (list (rassq 'jka-compr-handler file-name-handler-alist))))
                       (require req nil t)
                       nil)))
                  (push req elemacs-incremental-packages))
            (error
             (message "Error: failed to incrementally load %S because: %s" req e)
             (setq elemacs-incremental-packages nil)))
          (when packages
			(run-with-idle-timer elemacs-incremental-idle-timer
								 nil #'elemacs-load-packages-incrementally
								 elemacs-incremental-packages t)
			(setq packages nil)))))))


(defun elemacs-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `elemacs-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp elemacs-incremental-first-idle-timer)
    (if (zerop elemacs-incremental-first-idle-timer)
        (mapc #'require (cdr elemacs-incremental-packages))
      (run-with-idle-timer elemacs-incremental-first-idle-timer
                           nil #'elemacs-load-packages-incrementally
                           elemacs-incremental-packages t))))

(add-hook 'emacs-startup-hook #'elemacs-load-packages-incrementally-h)

(defun enlist (x)
  "Makesure the result is a list."
  (if (listp x) x (list x)))

(provide 'init-funcs)
;;; init-funcs.el ends here
