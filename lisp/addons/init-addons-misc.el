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


(setup mu4e
	(:load+ mu4e)
	(:also-load smtpmail)
	(:also-load epa-file)
	(:autoload mu4e)
	(:option*
	 mu4e-maildir "~/mail/"
	 mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
	 mu4e-update-interval 300 ;; in seconds
	 mu4e-attachment-dir "~/mail/attachment/"
	 mu4e-change-filenames-when-moving t
	 mu4e-user-mailing-lists '("mob5277@outlook.com")
	 mu4e-maildir-shortcuts '(("/outlook/INBOX" . ?o)
														("/outlook/Sent" . ?O)
														("/deku/INBOX" . ?d)
														("/deku/Sent" . ?D))
	 mu4e-context-policy 'pick-first
	 mu4e-compose-context-policy 'ask
	 mu4e-contexts
	 `(,(make-mu4e-context
			 :name "outlook"
			 :enter-func
			 (lambda () (mu4e-message "Enter mob5277@outlook.com context"))
			 :leave-func
			 (lambda () (mu4e-message "Leave mob5277@outlook.com context"))
			 :match-func
			 (lambda (msg)
				 (when msg
					 (mu4e-message-contact-field-matches msg :to "mob5277@outlook.com")))
			 :vars '((user-mail-address . "mob5277@outlook.com")
							 (user-full-name . "Mob")
							 (mu4e-drafts-folder . "/Drafts")
							 (mu4e-refile-folder . "/Archive")
							 (mu4e-sent-folder . "/Sent")
							 (mu4e-trash-folder . "/Deleted")))
		 ,(make-mu4e-context
			 :name "deku"
			 :enter-func
			 (lambda () (mu4e-message "Enter dekuofa1995@gmail.com context"))
			 :leave-func
			 (lambda () (mu4e-message "Leave dekuofa1995@gmail.com context"))
			 :match-func
			 (lambda (msg)
				 (when msg
					 (mu4e-message-contact-field-matches msg :to "dekuofa1995@gmail.com")))
			 :vars '((user-mail-address . "dekuofa1995@gmail.com")
							 (user-full-name . "dekuofa1995")
							 (mu4e-drafts-folder . "/Drafts")
							 (mu4e-refile-folder . "/Archive")
							 (mu4e-sent-folder . "/Sent")
							 (mu4e-trash-folder . "/Deleted"))))
	 )
	;; (add-to-list 'mu4e-bookmarks
	;; 						 (mu4e-bookmark-define "mail:/outlook/INBOX" "Inbox - outlook" ?o))
	)

(defun deku/set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "mob5277@outlook.com" from) "outlook")
               ((string-match "dekuofa1995@gmail.com" from) "deku")
               ;; ((string-match "dummy@example.com" from) "example")
							 )))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(setup epa-file
	(:when-loaded
		(epa-mail-mode)
		(auth-source-forget-all-cached)
		(add-hook 'message-send-mail-hook 'deku/set-msmtp-account))
	(:option*
	 epg-pinentry-mode 'loopback
	 message-kill-buffer-on-exit t
	 send-mail-function 'sendmail-send-it
	 message-send-mail-function 'sendmail-send-it
	 sendmail-program (executable-find "msmtp")
	 message-sendmail-envelope-from 'header))
(provide 'init-addons-misc)
;;; init-addons-misc.el ends here
