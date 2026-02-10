;;; init-tools.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup vundo
  (:global-bind
   "C-?" vundo)) ;; Emacs's undo binding on C-/, bind C-S-/ for vundo for more complex situations.

(setup avy
  (:when-loaded
    (custom-set-faces
     '(avy-lead-face   ((t (:foreground "#00dfff" :background "unspecified" :wegith 'bold))))
     '(avy-lead-face-0 ((t (:foreground "#2b8db3" :background "unspecified"))))
     '(avy-lead-face-1 ((t (:foreground "#2b8db3" :background "unspecified"))))
     '(avy-lead-face-2 ((t (:foreground "#2b8db3" :background "unspecified"))))))
  (:doc "Colemak layout keys.")
  (:option*
   avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
  (:global-bind
   "C-'"  avy-goto-char-2))

(setup expreg
	(:url "https://github.com/casouri/expreg")
	(:doc "Package just like expand-region.")
  (:global-bind
   "C-," 'expreg-expand
   "C-<" 'expreg-contract))

(setup paredit
  (:global-bind
   "C-c )" paredit-forward-slup-sexp
   "C-c (" paredit-forward-barf-sexp)
  (:hooks (list emacs-lisp-mode-hook
								clojure-mode-hook lisp-data-mode-hook)
					enable-paredit-mode)
  (:with-map paredit-mode-map
    (:bind
     "M-o" paredit-splice-sexp)
    (:unbind "C-<left>" "C-<right>" "M-s" "M-?" [?\r])))

(setup edit-indirect
	(:doc "Dependence of separedit."))

(setup separedit
	(:autoload separedit-mark-region)
	(:doc "Edit comment/string/docstring/code block in separate buffer with your favorite mode.")
	(:tag "edit")
	(:url "https://github.com/twlz0ne/separedit.el#edit-minibuffer")
	(:global-bind
	 "C-c C-'" separedit))

(setup rg)

(setup posframe)

(setup wgrep
  (:with-map grep-mode-map
    (:bind
     "C-c C-p" wgrep-change-to-wgrep-mode)))

(setup iedit
  (:global-bind
   "C-;" iedit-mode
   "C-M-;" iedit-rectangle-mode))

(setup multiple-cursors
	(:global-bind
	 "C-;" mc/mark-all-like-this
	 "C->" mc/mark-next-like-this
	 "C-c C-c" mc/edit-lines
	 "C-<" mc/mark-prev-like-this))

(setup rime
  (:global-bind
   "C-M-<return>" toggle-input-method) ;; orig. C-\
  (:with-map rime-mode-map
    (:bind
     "C-`" rime-send-keybinding))
  (:with-map rime-active-mode-map
    (:bind
     "C-t" rime-inline-ascii))
  (:option
   default-input-method "rime")
  (:option*
   rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "C-a" "C-e"
																"C-d" "M-v" "<left>" "<right>" "<up>"
																"<down>" "<prior>" "<next>" "<delete>")
   rime-inline-ascii-trigger 'shift-r
   rime-user-data-dir "~/Library/Rime/"
   rime-cursor         "˰"
   rime-show-candidate 'posframe
   rime-posframe-style 'vertical
   rime-show-preedit   t
   rime-librime-root (expand-file-name "librime" user-emacs-directory)
   default-input-method "rime"))

(setup sis
  (:option*
   sis-prefix-override-buffer-disable-predicates
   (list 'minibufferp
         (;; magit
					lambda ()
					(sis--string-match-p "^magit.*:" (buffer-name)))
         (;; special buffer
					lambda ()
					(let ((normalized-buffer-name
                 (downcase (string-trim (buffer-name)))))
            (and (sis--string-match-p "^\*" normalized-buffer-name)
                 (not (sis--string-match-p "^\*new\*" normalized-buffer-name))
                 (not (sis--string-match-p "^\*dashboard\*"
                                           normalized-buffer-name))
                 (not (sis--string-match-p "^\*scratch\*"
                                           normalized-buffer-name))))))
   sis-default-cursor-color "#51afef"
   sis-other-cursor-color   "#FF8000")
  (:when-loaded

    (sis-ism-lazyman-config "com.apple.keylayout.ABC"
                            "im.rime.inputmethod.Squirrel.Hans"
                            'native)
    ;; automatically change cursor color according to current input source.
    ;; bug: laggy
    (sis-global-cursor-color-mode  t))
  (:after meow
    (add-hook 'meow-insert-exit-hook #'sis-set-english)
    (add-hook 'focus-in-hook #'sis-set-english)))

(provide 'init-tools)

(setup eee
	(global-unset-key (kbd "s-e"))
	(defun switch-to-wezterm (&rest _)
    "Switch to WezTerm terminal."
    (interactive)
		(sleep-for 0.1)
    (do-applescript "
    tell application \"WezTerm\"
      activate
    end tell"))
	(:autoload ee-jump-from)
	;; (:advice ee-run :after switch-to-wezterm )
	(:option ee-terminal-command "ghostty")
	(:global-bind
   "s-e y" 'ee-yazi-project
	 "s-e Y" 'ee-yazi
	 "s-e f" 'ee-find
	 "s-e g" 'ee-lazygit
	 "s-e d" 'ee-delta
	 "s-e r" 'ee-rg
	 "s-e l" 'ee-line))

;;; init-tools.el ends here
