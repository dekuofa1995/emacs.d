;;; init-emacs-config.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setq-default
 tab-width 2
 inhibit-startup-screen t
 ;; Don't display comp warnings
 warning-suppress-log-types '((comp))
 ;; Don't create lockfiles
 create-lockfiles nil

 ;; prefer UTF8
 buffer-file-coding-system       'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system  'utf-8-unix
 default-process-coding-system   '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system  'utf-8-unix
 default-terminal-coding-system  'utf-8-unix

 ;; Add newline at bottom of file
 require-final-newline  t

 ;; Backup setups
 ;; We use              temporary directory /tmp for backup files
 ;; More versions should be saved
 backup-by-copying      t
 delete-old-versions    t
 version-control        t
 kept-new-versions 6
 kept-old-versions 2
 backup-directory-alist         `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

 ;; Skip prompt for xref find definition
 xref-prompt-for-identifier nil

 ;; Don't wait for keystrokes display
 echo-keystrokes 0.01

 ;; Disable margin for overline and underline
 overline-margin 0
 underline-minimum-offset 0

 ;; Allow resizing frame by pixels
 frame-resize-pixelwise t

 ;; Better scroll behavior
 mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
 mouse-wheel-progressive-speed nil

 ;; scroll margin
 scroll-margin 15
 scroll-step   1
 ;; Disable copy region blink
 copy-region-blink-delay 0

 ;; Use short answer when asking yes or no
 read-answer-short t

 ;; Always follow link
 vc-follow-symlinks t

 ;; Use custom.el as custom file
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 ;; Disable ring bell
 ring-bell-function 'ignore

 ;; Mouse yank at current point
 mouse-yank-at-point t

 ;; DWIM target for dired
 ;; Automatically use another dired buffer as target for copy/rename
 dired-dwim-target t

 ;; Don't echo multiline eldoc
 eldoc-echo-area-use-multiline-p nil)

;; fix chinese input lag from https://emacs-china.org/t/linux/12971/22
(set-language-environment 'utf-8)

(load custom-file)

;; enable a pair of parenthes match mode
(show-paren-mode t)

(global-subword-mode 1)
;; (require 'subword-mode)
(with-eval-after-load 'diminish
  (diminish 'subword-mode))

(global-hl-line-mode t)
(setq global-auto-revert-non-file-buffers t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; enable line numbers
(defun my/enable-line-numbers ()
  (interactive)
  (display-line-numbers-mode 1))
;; (add-hook 'prog-mode-hook #'my/enable-line-numbers)
;; (add-hook 'prog-mode-hook #'my/enable-line-numbers)
;; (global-display-line-numbers-mode 1)

(save-place-mode 1) ;; remember the last cursor location

;; (defun switch-to-new-window (&rest _) (other-window 1))
;; (advice-add #'split-window-below :after #'switch-to-new-window)
;; (advice-add #'split-window-right :after #'switch-to-new-window)

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(global-set-key (kbd "<f5>") 'save-buffer-always)
(global-set-key (kbd "C-x b") 'switch-to-minibuffer)

(setup topsy
  (:hooks prog-mode-hook topsy-mode))

(define-key global-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key global-map (kbd "<C-g>") 'keyboard-escape-quit)
(define-key global-map (kbd "s-D") 'find-file)
(define-key global-map [remap capitalize-word] #'capitalize-dwim)

(provide 'init-emacs-config)
;;; init-emacs-config.el ends here
