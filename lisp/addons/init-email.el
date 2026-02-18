;;; init-email.el --- setup for email functions -*- lexical-binding: t -*-

;;; Code:

(setup notmuch
	(defun my-notmuch-sync ()
		"Async Run mbsync then notmuch new."
		(interactive)
		(async-shell-command "mbsync -a && notmuch new" "*notmuch-sync*"))
	(:option* notmuch-search-oldest-first t))

(setup mu4e
	)



(provide 'init-email)
;;; init-email.el ends here
