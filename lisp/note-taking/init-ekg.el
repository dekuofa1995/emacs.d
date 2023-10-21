;;; init-ekg.el -- https://github.com/ahyatt/ekg -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup ekg
	(:autoload ekg-capture)
	(once (list :before #'ekg-capture)
		(require 'ekg))
	(:option
	 ekg-db-file-obsolete "~/.emacs.d/var/ekg-db/ekg.db")
	(:global
	 [f2] ekg-capture))


(provide 'init-ekg)
;;; init-ekg.el ends here
