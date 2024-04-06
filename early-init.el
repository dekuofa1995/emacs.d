;;; -*- lexical-binding: t -*-

;;; Code:
;; Adjust garbage collection thresholds during startup
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold most-positive-fixnum))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold))))

;; add load path
(dolist (path '("core" "lib/borg" "lib/benchmark-init" "lib/org/lisp"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))
(require 'init-const)
;;(require 'benchmark-init)
;;(add-hook 'after-init-hook 'benchmark-init/deactivate)
;;(setq native-comp-jit-compilation nil)
;; remove emacs title and make frame without rounded
(add-to-list 'default-frame-alist '(undecorated . t))
;; emacs-plus29+ configuration end
;; borg initialization
(setq package-enable-at-startup nil)
(setq package-archives nil)
(require 'borg)
(borg-initialize)

(setq use-package-always-defer t)
;; GCMH: the Garbage Collector Magic Hack
;;(require 'gcmh)
;;(gcmh-mode t)
