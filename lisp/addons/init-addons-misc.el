;;; init-addons-misc.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup imenu-list
  (:with-map imenu-list-major-mode-map
    (:bind "e" #'previous-line)))

(setup command-log-mode
  (:autoload global-command-log-mode)
  (:option*
   command-log-mode-auto-show t))

(setup magit
	(:load+ magit))

(setup diff-hl
  (:hooks (list prog-mode-hook conf-mode-hook) diff-hl-mode))


(setup vterm
	(defun +setup-vterm-font ()
		(set (make-local-variable 'buffer-face-mode-face) `(:family ,deku/term-font))
		(buffer-face-mode t))
  (:option*
   vterm-shell (if sys/macp "/usr/local/bin/fish" "/usr/bin/fish"))
	(:hooks vterm-mode-hook +setup-vterm-font)
  (:with-map vterm-mode-map
		(:unbind [next] [prior]) ;; enable centaur tabs switch
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

(defun try-term-keys ()
	(unless (display-graphic-p)
		(term-keys-mode t)))

(setup term-keys
	(:require term-keys)
	(:hooks after-init-hook try-term-keys))

(setup restclient
	(:once (list :files "http" )
		(require 'restclient))
	(:init
	 (add-to-list 'auto-mode-alist
								'("\\.http\\'" . restclient-mode))))

(progn
	;; tramp's configuration
	(defun sudo-find-file (file)
		"Open FILE as root."
		(interactive "FOpen file as root: ")
		(when (file-writable-p file)
			(user-error "File is user writeable, aborting sudo"))
		(find-file (if (file-remote-p file)
									 (concat "/" (file-remote-p file 'method) ":"
													 (file-remote-p file 'user) "@" (file-remote-p file 'host)
													 "|sudo:root@"
													 (file-remote-p file 'host) ":" (file-remote-p file 'localname))
								 (concat "/sudo:root@localhost:" file))))

	(defun sudo-this-file ()
		"Open the current file as root."
		(interactive)
		(sudo-find-file (file-truename buffer-file-name)))

	(setup tramp-mode
		(:option* tramp-terminal-type            "tramp"
							remote-file-name-inhibit-cache nil
							remote-file-name-inhibit-locks t
							trsamp-verbose                 0
							tramp-default-method           "ssh"
							tramp-auto-save-directory      temporary-file-directory)
		(:global
		 "C-x C-z" sudo-this-file)))

(defun try-term-keys ()
	(unless (display-graphic-p)
		(term-keys-mode t)))

(setup term-keys
	(:require term-keys)
	(:hooks after-init-hook try-term-keys))

(setup restclient
	(:once (list :files "http" )
		(require 'restclient))
	(:init
	 (add-to-list 'auto-mode-alist
								'("\\.http\\'" . restclient-mode))))
(provide 'init-addons-misc)
;;; init-addons-misc.el ends here
