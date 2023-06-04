;;; -*- lexical-binding: t -*-

;;; Code:
;; add load path
(dolist (path '("lisp" "lib/borg"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))
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
(require 'borg)
(borg-initialize)
(setq pacakge-archives nil)
;; (setq package-archives '(("gnu"   . "http://elpa.zilongshanren.com/gnu/")

;; 			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")))

;; (setq use-package-always-ensure t)

(use-package benchmark-init
  ;; :defer (not setup-benchmark)
  :diminish t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; GCMH: the Garbage Collector Magic Hack
(use-package gcmh
  :config
  (gcmh-mode 1))
