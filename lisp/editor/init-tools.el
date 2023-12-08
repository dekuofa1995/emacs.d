;;; init-tools.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup vundo
  (:global
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
  (:global
   "C-'"  avy-goto-char-2))

(setup expreg
	(:url "https://github.com/casouri/expreg")
	(:doc "Package just like expand-region.")
  (:global
   "C-," 'expreg-expand
   "C-<" 'expreg-contract))

(setup paredit
  (:global
   "C-c )" paredit-forward-slup-sexp
   "C-c (" paredit-forward-barf-sexp)
  (:hooks (list emacs-lisp-mode-hook
								clojure-mode-hook)
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
	(:global
	 "C-c C-'" separedit))

(setup rg)

(setup wgrep
  (:with-map grep-mode-map
    (:bind
     "C-c C-p" wgrep-change-to-wgrep-mode)))

(setup iedit
  (:global
   "C-;" iedit-mode
   "C-M-;" iedit-rectangle-mode))

(setup rime
  (:global
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
   rime-user-data-dir "~/.config/rime"
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

(setup realgud
  (:hooks
   realgud-short-key-mode-hook
   (lambda ()
     (local-set-key "\C-c" realgud:shortkey-mode-map))))

(provide 'init-tools)
;;; init-tools.el ends here
