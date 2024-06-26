;;; init-tools.el --  -*- lexical-binding: t -*-
;;; Commentary: programming tools
;;; Code:

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
	(:load-after treesit)
  (:when-loaded
		(add-hook 'prog-mode-hook #'treesit-auto-mode)
    (setq treesit-auto-install 'prompt)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(c-or-c++-mode . c-or-c++-ts-mode))
    (add-to-list 'auto-mode-alist
                 '("\\.ya?ml\\'" . yaml-ts-mode))))

(setup treesit-fold
	(:hooks prog-mode-hook treesit-fold-mode)
	(:with-map prog-mode-map
		(:bind
		 "C-x M-f" treesit-fold-close
		 "C-x C-M-f" treesit-fold-close-all
		 "C-x M-o" treesit-fold-open
		 "C-x C-M-o" treesit-fold-open-all)))

(setup flymake
	(:with-mode prog-mode
		(:hook flymake-mode))
	(:with-map flymake-mode-map
		(:bind
		 "C-c e ]" flymake-goto-next-error
		 "C-c e [" flymake-goto-prev-error
		 "C-c e b" flymake-show-buffer-diagnostics
		 ;; flymake use project.el
		 "C-c e p" flymake-show-project-diagnostics)))

(defun dape-startup ()
	(save-some-buffers t t))

(setup dape
	(:hooks deape-on-start-hook dape-startup)
	(:options dape-buffer-window-arrangement 'right
					  dape-cwd-fn 'projectile-project-root))

(provide 'init-tools)
;;; init-tools.el ends here
