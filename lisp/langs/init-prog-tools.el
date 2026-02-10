;;; init-tools.el --  -*- lexical-binding: t -*-
;;; Commentary: programming tools
;;; Code:


(setup treesit
  (:when-loaded
    (defun mp-setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar
               '((css "https://github.com/tree-sitter/tree-sitter-css")
								 (clojure . ("https://github.com/sogaiu/tree-sitter-clojure" "master" "src"))
								 (json . ("https://github.com/tree-sitter/tree-sitter-json" "master" "src"))
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                 (python "https://github.com/tree-sitter/tree-sitter-python")
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                 (yaml "https://github.com/ikatyang/tree-sitter-yaml")
								 (typst "https://github.com/uben0/tree-sitter-typst")))
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
		(dolist (mapping '((python-mode . python-ts-mode)
											 (c-mode . c-ts-mode)
											 (c++-mode . c++-ts-mode)
											 (rust-mode . rust-ts-mode)
											 (c-or-c++-mode . c-or-c++-ts-mode)))
			(add-to-list 'major-mode-remap-alist mapping))
		(dolist (mapping '(("\\.js\\'" . js-mode)
											 ("\\.ts\\'" . typescript-ts-mode)
											 ("\\.jsx\\'" . js-mode)
											 ("\\.tsx\\'" . tsx-ts-mode)
											 ;; ("\\.json\\'" . json-ts-mode)
											 ))
			(add-to-list 'auto-mode-alist mapping))
    (add-to-list 'auto-mode-alist
                 '("\\.ya?ml\\'" . yaml-ts-mode))))

(setup treesit-fold
	(:hooks prog-mode-hook treesit-fold-mode)
	(:with-map prog-mode-map
		(:bind
		 "M-s-[" treesit-fold-close
		 "M-s-{" treesit-fold-close-all
		 "M-s-]" treesit-fold-open
		 "M-s-}" treesit-fold-open-all)))

(setup flymake
	(:option*
	 flymake-show-diagnostics-at-end-of-line 'short
	 flymake-no-changes-timeout 30)
	(:with-mode eglot-mode
		(:hook flymake-mode))
	(:with-map flymake-mode-map
		(:bind
		 "C-c C-e ]" flymake-goto-next-error
		 "C-c C-e [" flymake-goto-prev-error
		 "C-c C-e b" flymake-show-buffer-diagnostics
		 ;; flymake use project.el
		 "C-c C-e p" flymake-show-project-diagnostics))
	)

(setup flymake-aspell
	(:hooks (text-mode-hook
					 prog-mode-hook) flymake-aspell-setup)
	(:option* ispell-program-name "aspell"
						ispell-silently-savep t))

(setup dape
	(:option* dape-buffer-window-arrangement 'right
					  dape-cwd-fn 'projectile-project-root)
	(defun dape-startup ()
		(save-some-buffers t t))
	(:hooks dape-on-start-hook dape-startup))

(setup citre
	(:once (list :hooks 'prog-mode-hook 'emacs-lisp-mode-hook)
		(require 'citre))
	(:also-load citre-config)
	(:global-bind
	 "C-x c j" citre-jump
	 "C-x c r j" citre-jump-to-reference
	 "C-x c J" citre-jump-back
	 "C-x c p" citre-ace-peek
	 "C-x c P" citre-peek
	 "C-x c u" citre-update-this-tags-file)
	(:option*
	 citre-project-root-function #'projectile-project-root
	 citre-default-create-tags-file-location 'global-cache
	 citre-edit-ctags-options-manually nil
	 citre-auto-enable-citre-mode-modes '(prog-mode))
	(:when-loaded
		(defvar citre-elisp-backend
			(citre-xref-backend-to-citre-backend
			 ;; This is the xref backend name
			 'elisp
			 ;; A function to tell if the backend is usable
			 (lambda () (derived-mode-p 'emacs-lisp-mode))))
		(citre-register-backend 'elisp citre-elisp-backend)
		(setq
		 citre-find-definition-backends '(elisp eglot tags global)
		 citre-find-reference-backends '(elisp eglot global))))

(provide 'init-prog-tools)
;;; init-tools.el ends here
