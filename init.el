;;; init.el --- org configurations -*- lexical-binding: t -*-
;;; Commentary:

;; load customization

;;; constants

;;; Code:
(dolist (path '("lisp" ".my-config"))
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(require 'cl-lib)
(require 'init-const)
(require 'init-funcs)
;; fix: variable personal-keybindings missing, which is needed by `use-package'
;; (require 'bind-key)

(setq use-package-always-defer t)
;; laf: Look and feel most for theme and font
;; editor: Basic editor
;; note-taking: For note taking confiugrations. such as org and org-roam
;; programming: lsp, snippet, completion and more for programming
;; addons: other kinds of packages cannot put into below files, such magit(for vc) and emacs-rime
;; privates: emacs custom properties and keybindings

(defun my/config-refresh ()
  "Force refresh all my config files."
  (interactive)
  (dolist (name my/config-name-list) (my/config-load name t)))
(global-set-key (kbd "<f12>") #'my/config-refresh)

;; (unless custom-enabled-themes
;;   (meomacs-load-theme))

;; load laf and private at early-init.el
(require 'private)
(require 'laf)
(require 'editor)
(require 'programming)
(require 'note-taking)
(require 'addons)
;; (my/config-load "private" my/config-force-reload)
;; (my/config-load "laf" my/config-force-reload)
;; (my/config-load "editor" my/config-force-reload)
;; (my/config-load "note-taking" my/config-force-reload)
;; (my/config-load "programming" my/config-force-reload)
;; (my/config-load "addons" my/config-force-reload)
(add-hook 'dashboard-mode-hook (lambda ()
				 (load-theme 'kaolin-light)))

(provide 'init)
;;; init.el ends here
