;;; -*- lexical-binding: t -*-

;;; Code:
;; add load path
(push (expand-file-name "lisp" user-emacs-directory) load-path)
(require 'init-const)
(require 'init-funcs)

;; Adjust garbage collection thresholds during startup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold most-positive-fixnum))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold))))
;; emacs-plus29+ configuration start
(defvar native-comp-deferred-compilation-deny-list nil)
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated-round . t))
;; emacs-plus29+ configuration end
;; borg initialization
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;; (defvar my/config-backup-dir ".my-backup")
;; (defvar my/config-dir ".my-config")

;; (defun my/get-elisp-file-name (config-name)
;;   "Return the given CONFIG-NAME's elisp file  name."
;;   (format "%s.el" config-name))
;; (defun my/get-org-file-name (config-name)
;;   "Return the given CONFIG-NAME's org file name."
;;   (format "%s.org" config-name))

;; (defun my/config-backup (config-name target)
;;   "Backup config file by given CONFIG-NAME.
;; TARGET is source file path.
;; Store the backup file to `user-emacs-directory' + MY/CONFIG-BACKUP-DIR"
;;   (let* ((backup-dir (expand-file-name my/config-backup-dir user-emacs-directory))
;; 	 (backup-file (expand-file-name (my/get-elisp-file-name config-name) backup-dir)))
;;     (when (file-exists-p target)
;;       (unless (file-exists-p backup-dir) (make-directory backup-dir))
;;       (copy-file target backup-file t))))

;; (defun my/config-load (config-name &optional force)
;;   "Load my custom configuration file by given CONFIG-NAME.
;; If FORCE NON-NIL then generate file wheather the old file exist or not
;; First try backup file, then generate new elisp file"
;;   (let* ((org-file(expand-file-name (my/get-org-file-name config-name) user-emacs-directory))
;; 	 (target-dir (expand-file-name my/config-dir user-emacs-directory))
;; 	 (target-file (expand-file-name (my/get-elisp-file-name config-name) target-dir)))
;;     (when (file-exists-p org-file)
;;       (make-directory target-dir t)
;;       (when (or force (not (file-exists-p target-file)))
;; 	(require 'org)
;; 	(require 'ob)
;; 	(my/config-backup config-name target-file)
;; 	(org-babel-tangle-file org-file target-file))
;;       (load-file target-file))))

(setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")

			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

;; (defvar bootstrap-version)

;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
;; (setq straight-use-package-by-default t)

;; (straight-use-package 'use-package)
;; (straight-use-package 'org)

(setq use-package-always-ensure t)
(setq straight-vc-git-default-clone-depth 1)

(use-package benchmark-init
  :ensure t
  :diminish t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;; GCMH: the Garbage Collector Magic Hack
(use-package gcmh
  :config
  (gcmh-mode 1))
;; (load-file (expand-file-name "lisp/pair.el" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name "lisp/pair" user-emacs-directory))
