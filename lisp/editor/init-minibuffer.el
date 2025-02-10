;;; init-minibuffer.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup consult
  (:load-after vertico)
  (:option* consult-async-input-debounce 0.8
						consult-async-input-throttle 0.3)
	(:after transient
		(transient-define-prefix deku/trans-consult-map ()
			"Consult command map."
			["Transient menu for consult commands"
			 ["Emacs"
				("e x" "run commands from any modes" consult-mode-command)
				("e i" "search info MANUALS" consult-info)
				("e m" "search MAN page" consult-man)
				("e h" "history" consult-history)
				("e s" "isearch histroy" consult-isearch-history)
				("e c" "complex CMD" consult-complex-command)
				("c e" "compile error" consult-compile-error)
				("e e" "flymake error" consult-flymake)
				("b" "bookmark" consult-bookmark)
				("m k" "mark" consult-mark)
				("M" "global mark" consult-global-mark)
				]
			 ["Search"
				("f" "find file" consult-find)
				("g" "grep" consult-grep)
				("G" "git grep" consult-git-grep)
				("r g" "ripgrep" consult-ripgrep)
				("l" "line" consult-line)
				("m l" "multi-line" consult-line-multi)
				("." "find references" xref-find-references)
				("K" "keep lines" consult-keep-lines)
				("F" "focus lines" consult-focus-lines)
				("M-o" "org-heading" consult-org-heading)
				("o" "outline" consult-outline)
				("i" "imenu" consult-imenu)
				("m i" "multi-imenu" consult-imenu-multi)
				]
			 ["Register"
				("r r" "register" consult-register )
				("r l" "load register" consult-register-load)
				("r s" "store register" consult-register-store)
				]
			 ])
		(:global
		 "M-g" deku/trans-consult-map))
  (:global
   [remap switch-to-buffer] 	consult-buffer
   [remap Info-search]      	consult-info
	 [remap goto-line] 	consult-goto-line
   "M-y"     	consult-yank-pop)
  (:with-map
      isearch-mode-map
    (:bind
     "M-e"   	consult-isearch-history
     "M-s e" 	consult-isearch-history
     "M-s l" 	consult-line
     "M-s L"	consult-line-multi))
  (:with-map minibuffer-local-map
    (:bind
     "M-s"  	consult-history
     "M-r"  	consult-history))
  (:hooks completion-list-mode-hook consult-preview-at-point-mode)
	(:doc "Optionally tweak the register preview window.
This adds thin lines, sorting and hides the mode line of the window.")
  (:advice register-preview :override consult-register-window)
  (:when-loaded
    (setq register-preview-delay 2
					register-preview-function #'consult-register-format)
    (setq xref-show-xrefs-function #'consult-xref
					xref-show-definitions-function #'consult-xref)
    (consult-customize
     ;; consult-theme :preview-key '(:debounce 0.4 any)
     ;; consult-ripgrep consult-git-grep consult-grep
     ;; consult-bookmark consult-recent-file consult-xref
     ;; consult--source-bookmark consult--source-file-register
     ;; consult--source-recent-file consult--source-project-recent-file
     :preview-key '(:debounce 0.4 any))))

(setup consult-projectile
  (:after projectile
    (:after consult
      (require 'consult-projectile)))
  (:global
   "M-s p" consult-projectile))

(setup consult-eglot
  (:after consult
    (:after eglot
      (require 'consult-eglot)))
  (:with-map eglot-mode-map
    (:bind
     "M-s d" consult-eglot-symbols)))

(setup hl-todo
	(:doc "The dependence of consult-todo.")
	(:once (list :hooks 'buffer-list-update-hook)
		(global-hl-todo-mode t)))

(setup consult-todo
	(:doc "Search keywords such as todo in buffer(s).")
	(:url "https://github.com/liuyinz/consult-todo")
	(:tag "consult" "todo")
	(:option*
	 consult-todo-only-comment t)
	(:global
	 "M-g t t" consult-todo
	 "M-g t a" consult-todo-all
	 "M-g t d" consult-todo-dir
	 "M-g t p" consult-todo-project))

(setup vertico
	(:also-load vertico-multiform vertico-prescient)
  (:once (list :hooks 'pre-command-hook)
    (vertico-mode 1))
  (:with-map vertico-map
    (:bind
     "C-<return>" vertico-exit-input))
  (:when-loaded
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))))

(setup vertico-multiform
  (:option*
	 vertico-multiform-commands '(`(consult-imenu buffer indexed)
																`(execute-extended-command unobtrusive)
																`(consult-outline buffer ,(lambda (_) (text-scale-set -1))))

	 ;; Configure the display per completion category.
	 ;; Use the grid display for files and a buffer
	 ;; for the consult-grep commands.
	 vertico-multiform-categories '((file grid)
																	(embark-keybinding grid)))
	(:when-loaded
		(vertico-multiform-mode)))

(setup vertico-prescient
  (:when-loaded
		(vertico-prescient-mode)))

(setup prescient
  (:autoload prescient-persist-mode)
  (:once (list :packages 'vertico)
    (prescient-persist-mode))
  (:hooks corfu-mode-hook corfu-prescient-mode))

(defun meomacs-backward-delete-sexp ()
  "Backward delete sexp.

Used in minibuffer, replace the the default kill behavior with M-DEL."
  (interactive)
  (save-restriction
    (narrow-to-region (minibuffer-prompt-end) (point-max))
    (delete-region
     (save-mark-and-excursion
       (backward-sexp)
       (point))
     (point))))
(define-key minibuffer-local-map (kbd "M-DEL") #'meomacs-backward-delete-sexp)

;; Enable rich annotations using the Marginalia package
(setup marginalia
  ;;
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  (:with-map minibuffer-local-map
    (:bind
     "M-A" marginalia-cycle))
  (:doc "Must be in the :init section of use-package such that the mode gets
   enabled right away. Note that this forces loading the package.")
  (:once (list :hooks after-init-hook)
    (marginalia-mode))
  :init
  (require 'marginalia)
  (marginalia-mode))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
