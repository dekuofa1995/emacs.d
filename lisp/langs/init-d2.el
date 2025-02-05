;;; init-d2.el -- D2 configuration for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setup d2-mode
	(:url "https://github.com/andorsk/d2-mode?tab=readme-ov-file")
	(:doc "A d2 extension for Emacs. This was heavily inspired of Mermaid Mode.")
	(:option* d2-output-format ".svg")
	(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode)))

(provide 'init-d2)
;;; init-d2.el ends here
