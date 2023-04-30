;;; init.el --- org configurations -*- lexical-binding: t -*-

;; load customization

;;; constants
(require 'cl-lib)
;; laf: Look and feel most for theme and font
;; editor: Basic editor
;; note-taking: For note taking confiugrations. such as org and org-roam
;; programming: lsp, snippet, completion and more for programming
;; addons: other kinds of packages cannot put into below files, such magit(for vc) and emacs-rime
;; privates: emacs custom properties and keybindings
(defvar my/config-name-list '(laf editor note-taking programming addons private))

(defun my/config-refresh ()
  "Force refresh all my config files"
  (interactive)
  (dolist (name my/config-name-list) (my/config-load name t)))

(global-set-key (kbd "<f12>") #'my/config-refresh)

(unless custom-enabled-themes
  (meomacs-load-theme))

;; load laf and private at early-init.el
(my/config-load "editor" t)
(my/config-load "note-taking" t)
(my/config-load "programming" t)
(my/config-load "addons" t)
(provide 'init)
