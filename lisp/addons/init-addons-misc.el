;;; init-addons-misc.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup imenu-list
  (:with-map imenu-list-major-mode-map
    (:bind "e" #'previous-line))
  (:with-map toggle-map
    (:bind "i" imenu-list)))

(setup command-log-mode
  (:autoload global-command-log-mode)
  (:option*
   command-log-mode-auto-show t)
  (:with-map toggle-map
    (:bind
     "c" global-command-log-mode)))

(setup magit
	(:load+ magit))

(setup diff-hl
  (:hooks (list prog-mode-hook conf-mode-hook) diff-hl-mode))

(setup which-key
  (:option*
   which-key-sort-order 'which-key-key-order-alpha)
  (:hooks after-init-hook which-key-mode))

(setup vterm
  (:option*
   vterm-shell (if sys/macp "/usr/local/bin/fish" "/usr/bin/fish"))
  (:with-map vterm-mode-map
    (:bind
     "C-y" #'my/vterm-send-C-y))
  (:init
   (defun my/vterm-send-C-y ()
     (interactive)
     (vterm-send-key (kbd "C-y"))))
  (:when-loaded
    ;; disable hl-line in vterm which will cause splash
    (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
    (add-hook 'vterm-copy-mode-hook (lambda () (call-interactively 'hl-line-mode)))))

(setup helpful
  (:global
   [remap describe-command] #'helpful-command
   [remap describe-function] #'helpful-callable
   [remap describe-variable] #'helpful-variable
   [remap describe-key] #'helpful-key
   "C-h M" #'helpful-macro) ;; very useful command to learn
  (:when-loaded
    ;; fix llama always show first
    (defun my-helpful-callable (symbol)
      (interactive
       (list (helpful--read-symbol
              "Callable: "
              (helpful--callable-at-point)
              (lambda (sym)
		(and (not (string-empty-p (symbol-name sym)))
                     (fboundp sym))))))
      (helpful--update-and-switch-buffer symbol t))
    (advice-add 'helpful-callable :override #'my-helpful-callable)
    ;; (define-key global-map [remap describe-function] #'my-helpful-callable)
    (define-key helpful-mode-map (kbd "e") 'backward-button)))

(provide 'init-addons-misc)
;;; init-addons-misc.el ends here
