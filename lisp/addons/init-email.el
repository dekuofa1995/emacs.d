;;; init-email.el --- setup for email functions -*- lexical-binding: t -*-

;;; Code:

(setup notmuch
	(defun my-notmuch-sync ()
		"Async Run mbsync then notmuch new."
		(interactive)
		(async-shell-command "mbsync -a && notmuch new" "*notmuch-sync*"))
	(:option* notmuch-search-oldest-first t))

(setup mu4e
	(:load+ mu4e)
	(defun password-from-keychain (service account)
		(string-trim
		 (shell-command-to-string
			(format "security find-generic-password -s %s -a %s -w 2>/dev/null"
							(shell-quote-argument service)
							(shell-quote-argument account)))))
	(:option* mail-user-agent 'mu4e-user-agent
						mu4e-maildir "~/.mail"
						mu4e-get-mail-command "mbsync -a && mu index"
						smtpmail-smtp-server "smtp.gmail.com"
						smtpmail-smtp-service 587
						smtpmail-stream-type 'starttls
						user-mail-address "dekuofa1995@gmail.com"
						smtpmail-smtp-password
						(password-from-keychain "mbsync-dekuofa" "dekuofa1995@gmail.com")))



(provide 'init-email)
;;; init-email.el ends here
