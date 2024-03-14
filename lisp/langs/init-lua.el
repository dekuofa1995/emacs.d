;;; init-lua.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; lua mode for basic editing
(setup lua-mode
	(:autoload #'lua-mode)
	(:hooks lua-mode-hook eglot-ensure)
	(:init
	 (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
	 (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))

(provide 'init-lua)
;;; init-lua.el ends here
