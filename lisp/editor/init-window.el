;;; init-window.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defmacro my/window-select-or-expand* ()
  "Define functions for window-select and expand in the normal state of meow-mode"
  `(progn
     ,@(cl-loop for x to 9
                collect
                `(defun ,(read (format
                                "select-window-or-expand-%d"
                                x))
                     (&rest _arg)
                   (interactive "P")
                   (if (region-active-p)
                       ( ,(read (format  "meow-expand-%d" x)))
                     ( ,(read (format "select-window-%d" x))))))))

(with-eval-after-load 'window-numbering
  (with-eval-after-load 'meow
    (my/window-select-or-expand*)))

(setup winum
  (:once (list :hooks 'dashboard-mode-hook)
    (winum-mode t)))

(with-eval-after-load 'winum
  (with-eval-after-load 'meow
    (defmacro deku/window-select-or-expand* ()
      "Define functions for window-select and expand in the normal state of meow-mode"
      `(progn
         ,@(cl-loop for x to 9
                    collect
                    `(defun
                         ,(read (format "select-window-or-expand-%d" x))
                         (&rest _arg)
                       (interactive "P")
                       (if (region-active-p)
                           ( ,(read (format  "meow-expand-%d" x)))
                         (winum-select-window-by-number ,x))))))
    (deku/window-select-or-expand*)))

(defvar deku/popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Warnings\\*"
          "\\*Embark Actions\\*"
          "\\*Calendar\\*"
          "\\*Finder\\*"
          "\\*King Ring\\*"
          "\\*Apropos\\*"
          "\\*Pp Eval Output\\*$"

          compilation-mode
          bookmark-bmenu-mode
          comint-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode rg-mode deadgrep-mode ;; ag-mode pt-mode occur-mode

          "^\\*Process List\\*" process-menu-mode
          cargo-process-mode

          ;; "^\\*eshell.*\\*.*$"       eshell-mode
          ;; "^\\*shell.*\\*.*$"        shell-mode
          ;; "^\\*terminal.*\\*.*$"     term-mode
          "^\\*vterm[inal]*.*\\*.*$" vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Org Src\\*"
          ;; "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode))

(setup popper
  (:once (list :hooks 'find-file-hook)
    (popper-mode)
    (popper-echo-mode))
  (:init
   (setq popper-echo-dispatch-actions nil))
  (:autoload popper-group-by-directory
	     popper-toggle-latest
	     popper-cycle
	     popper-toggle-type)
  ;; (:option*
  ;;  popper-echo-dispatch-actions t)
  (:global
   "C-h z"     popper-toggle-latest
   "C-<tab>"   popper-cycle
   "C-M-<tab>" popper-toggle-type)
  ;; (:hooks emacs-startup-hook popper-mode)
  (:when-loaded

    (setq popper-group-function #'popper-group-by-directory)
    (setq popper-reference-buffers deku/popper-reference-buffers)
    (popper-echo-mode t)
    (with-no-warnings
      (defun my-popper-fit-window-height (win)
	"Determine the height of popup window WIN by fitting it to the buffer's content."
	(fit-window-to-buffer
	 win
	 (floor (frame-height) 3.3)
	 (floor (frame-height) 3.3)))
      (setq popper-window-height #'my-popper-fit-window-height)

      (defun popper-close-window-hack (&rest _)
	"Close popper window via `C-g'."
	;; `C-g' can deactivate region
	(when (and (called-interactively-p 'interactive)
                   (not (region-active-p))
                   popper-open-popup-alist)
          (let ((window (caar popper-open-popup-alist)))
            (when (window-live-p window)
              (delete-window window)))))
      (advice-add #'keyboard-quit :before #'popper-close-window-hack))))

(with-eval-after-load 'doom-modeline
  (setq popper-mode-line
        '(:eval (let ((face (if (doom-modeline--active)
                                'mode-line-emphasis
                              'mode-line-inactive)))
                  (if (and (icons-displayable-p)
                           (bound-and-true-p doom-modeline-mode))
                      (format " %s "
                              (nerd-icons-octicon "nf-oct-pin" :face face))
                    (propertize " POP" 'face face))))))

(setup centaur-tabs
  (:option*
   centaur-tabs-style                "wave"
   centaur-tabs-set-icons            t   ;; need all-the-icons
   centaur-tabs-set-close-button     nil
   centaur-tabs-set-modified-marker  t
   centaur-tabs-gray-out-icons  'buffer
   centaur-tabs-set-bar         'under
   centaur-tabs-cycle-scope     'tabs)
  (:hooks
   dashboard-mode-hook  centaur-tabs-local-mode
   calendar-mode-hook  centaur-tabs-local-mode)
  (:global
   ;; (setq centaur-tabs-set-icons nil)
   ;; <next>/<prior> scroll-down/up-command can use the alternative key: C/M-v
   "<prior>"  centaur-tabs-backward
   "<next>"   centaur-tabs-forward
   "C-c t c"  centaur-tabs-close-selected
   "C-c t C"  centaur-tabs-close-unselected
   "C-c t s"  centaur-tabs-switch-group
   "C-c t p"  centaur-tabs-group-by-projectile-project ;; need projectile
   "C-c t g"  centaur-tabs-group-buffer-groups) ;; use user's group configuration
  (:when-loaded
    ;; (centaur-tabs-change-fonts "Menlo" 180)
    (centaur-tabs-mode t)
    (setq x-underline-at-decent-line t) ;; in order to display the unberline of centaur-tabs
    (defvar my--centaur-vc-modes
      '(magit-blame-mode magit-blob-mode magit-diff-mode
			 magit-file-mode magit-log-mode
			 magit-process-mode magit-status-mode))

    (defvar my--centaur-text-modes
      '(org-mode org-agenda-clockreport-mode org-src-mode
		 org-agenda-mode org-agenda-log-mode
		 ;; org-beamer-mode org-indent-mode org-bullets-mode
		 ;; org-cdlatex-mode
		 diary-mode))
    (defun my-centaur-tabs-buffer-groups ()
      (let ((bn (buffer-name)))
	(list
	 (cond
          ((memq major-mode '(helpful-mode help-mode))
           "Help")
          ((derived-mode-p 'prog-mode 'cider-repl-mode)
           "Programming")
          ((derived-mode-p 'dired-mode)
           "Dired")
          ((memq major-mode my--centaur-vc-modes)
           "Magit")
          ;; Shell
          ;; ((memq major-mode
          ((memq major-mode '(eshell-mode shell-mode vterm-mode))
           "Shell")
          ;; Text modes: like org-mode
          ((or (string-prefix-p "*Org Src" bn)
               (string-prefix-p "*Org Export" bn)
               (memq major-mode my--centaur-text-modes))
           "Text")
          ((string-prefix-p "*" bn)
           "Emacs")
          (t
           (centaur-tabs-get-group-name (current-buffer))))))))
  (advice-add 'centaur-tabs-buffer-groups :override 'my-centaur-tabs-buffer-groups))

(setup olivetti
  (:with-map toggle-map
    (:bind
     "w" olivetti-mode))) ;; write-mode

(provide 'init-window)
;;; init-window.el ends here
