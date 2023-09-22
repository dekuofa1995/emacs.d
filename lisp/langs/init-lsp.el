;;; init-lsp.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup eglot
  (:option*
   eglot-ignored-server-capabilites '(:documentHighlightProvider)
   read-process-output-max (* 256 1024))
  (:global
   "C-M-r"	eglot-rename
   "<C-return>" eglot-code-actions
   ;; "C-M-f"	eglot-code-action-quickfix
   "C-c C-l"	eglot-code-action-line
   "C-c C-e"	eglot-code-action-extract
   "C-c C-f"	eglot-format
   "C-c C-o"	eglot-code-action-organize-imports
   "C-c C-h"  eldoc))

(defun deku/eglot-auto-format ()
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format)))

(setup treesit
  (:hooks prog-mode-hook (lambda () (require 'treesit)))
  (:when-loaded
    (defun mp-setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar
               '((css "https://github.com/tree-sitter/tree-sitter-css")
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                 (python "https://github.com/tree-sitter/tree-sitter-python")
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                 (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
        (add-to-list 'treesit-language-source-alist grammar)
        ;; Only install `grammar' if we don't already have it
        ;; installed. However, if you want to *update* a grammar then
        ;; this obviously prevents that from happening.
        (unless (treesit-language-available-p (car grammar))
          (treesit-install-language-grammar (car grammar)))))

    (mp-setup-install-grammars)))

(setup treesit-auto
  (:once (list :packages 'treesit)
    (require 'treesit-auto)
    (add-hook 'prog-mode-hook #'treesit-auto-mode))
  (:when-loaded
    (setq treesit-auto-install 'prompt)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(c-or-c++-mode . c-or-c++-ts-mode))
    (add-to-list 'auto-mode-alist
                 '("\\.ya?ml\\'" . yaml-ts-mode))))

(provide 'init-lsp)
;;; init-lsp.el ends here