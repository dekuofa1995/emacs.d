;;; init-ekg.el -- https://github.com/ahyatt/ekg -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setup ekg
	(:autoload ekg-capture
						 ekg-capture-url
						 ekg-show-notes-for-today
						 ekg-show-notes-with-any-tags
						 ekg-show-notes-with-all-tags)
	(once (list :before #'ekg-capture)
		(require 'ekg))
	(:with-map ekg-edit-mode-map
		(:unbind "C-c C-c")
		(:bind
		 "C-c C-'" ekg-edit-finalize))
	(:when-loaded
		(setq ekg-db-file-obsolete "~/.emacs.d/var/ekg-db/ekg.db"))
	(:after transient
		(transient-define-prefix transient-map-ekg ()
  "EKG."
  [["CAPTURE"
    ("cl" "url"      ekg-capture-url)
    ("cc" "capture"  ekg-capture)
		"EDIT"
		("rt" "rename"   ekg-global-rename-tag)]
   ["SHOW"
    "🢆 LIST"
		("lf" "all tag"  ekg-show-notes-with-all-tags)
		("ll" "any tag"  ekg-show-notes-with-any-tags)
		("lt" "trash"    ekg-show-notes-in-trash)
		("ld" "draft"    ekg-show-notes-in-drafts)
    "🢆 TIMED"
    ("tt" "today"    ekg-show-notes-for-today)
    ("tm" "mod"      ekg-show-notes-latest-modified)
    ("tc" "kap"     ekg-show-notes-latest-captured)]
   ["DB"
    ("dc" "close"    ekg-close)
    ("dd" "clean"    ekg-clean-db)
    ("du" "upgrade"  ekg-force-upgrade)]])
		(:global
		 [f2] transient-map-ekg)))


(provide 'init-ekg)
;;; init-ekg.el ends here
