;;; init.el --- org configurations -*- lexical-binding: t -*-
;;; Commentary:

;; load customization

;;; constants

;;; Code:
(eval-when-compile
  (dolist (path '("lisp"))
    (add-to-list 'load-path (expand-file-name path user-emacs-directory)))
  (require 'init-const))
(require 'cl-lib)
(require 'init-funcs)

(global-set-key (kbd "<f12>") #'deku/config-refresh)

(require 'git-ml)
(deku/load-all-init-files)

(if (daemonp)
    (add-hook 'server-after-make-frame-hook
	      #'deku/load-theme)
  (deku/load-theme))

(provide 'init)
;;; init.el ends here
