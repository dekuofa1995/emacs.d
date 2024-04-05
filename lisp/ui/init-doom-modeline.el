;;; init-doom-modeline.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup doom-modeline
  (:hook-into after-init-hook)
  (:option
   doom-modeline-project-detection        'ffip
   doom-modeline-icon                      t
   doom-modeline-major-mode-color-icon     t
   doom-modeline-buffer-state-icon         t
   doom-modeline-buffer-modification-icon  t
   doom-modeline-unicode-fallback          t
   doom-modeline-support-imenu             t
   doom-modeline-modal                     t
   doom-modeline-lsp                       t
   doom-modeline-indent-info               t
   doom-modeline-env-version               t
   doom-modeline-env-python                t
   doom-modeline-env-rust                  t
   doom-modeline-github                    t
   doom-modeline-enable-word-count         t
   doom-modeline-height                    1
   doom-modeline-buffer-encoding           nil
   doom-modeline-indent-info               nil
   doom-modeline-buffer-file-name-style 'relative-to-project
   doom-modeline-env-python-executable "python"
   doom-modeline-env-rust-executable   "rustc"
   doom-modeline-env-load-string       "..."
   doom-modeline-continuous-word-count-modes '(markdown-mode org-mode text-mode))
  (:when-loaded

    (doom-modeline-def-segment git-status
      "Display git status such as the number of modified files and the number of untracked files."
      (when-let ((git-status git-ml))
				(concat
				 (doom-modeline-spc)
				 ;; TODO colorful `git-status'
				 (doom-modeline-display-text (format "[%s]" (string-trim git-status)))
				 (doom-modeline-spc))))

    (doom-modeline-def-modeline 'deku-mode-line
      '(window-number workspace-name modals matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info minor-modes objed-state gnus debug lsp minor-modes
                  input-method buffer-encoding major-mode process vcs git-status check))
    (defun deku/setup-doom-modeline ()
      "Set `deku-mode-line' to default modeline."
      (doom-modeline-set-modeline 'deku-mode-line 'default))

    (doom-modeline-mode 1))
  (:hook #'deku/setup-doom-modeline))

;; (with-eval-after-load 'doom-modeline
;;   (doom-modeline-def-modeline 'deku-mode-line
;;       '(window-number workspace-name modals matches buffer-info remote-host buffer-position parrot selection-info)
;;       '(misc-info minor-modes objed-state gnus debug lsp minor-modes
;;                   input-method buffer-encoding major-mode process vcs git-status checker))
;;   (defun deku/setup-doom-modeline ()
;;     "Set `deku-mode-line' to default modeline."
;;     (doom-modeline-set-modeline 'deku-mode-line 'default))

;;   (add-hook 'doom-modeline-mode-hook 'deku/setup-doom-modeline))

(custom-set-faces
 '(mode-line ((t (:height 1.0)))))

(setup git-ml
  (:autoload #'git-ml-activate #'git-ml-refresh)
  (:with-feature doom-modeline
    (:hooks find-file-hook git-ml-activate)
    (:advice vc-refresh-state :after git-ml-refresh)))

(custom-set-faces
 '(meow-insert-indicator ((t (:background "#acf2bd" :foreground "black"))))
 '(meow-beacon-indicator ((t (:background "#FF8800" :foreground "white"))))
 '(meow-keypad-indicator ((t (:background "#ffc86f" :foreground "white"))))
 '(meow-motion-indicator ((t (:background "#51afef" :foreground "white"))))
 '(meow-normal-indicator ((t (:background "#51afef" :foreground "white"))))
 '(meow-search-indicator ((t (:background "#c678dd" :foreground "white")))))

(provide 'init-doom-modeline)
;;; init-doom-modeline.el ends here
