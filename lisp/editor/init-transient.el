;;; init-transient.el -- setup transient and define transient maps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare transient-map-toggle)

(setup transient
  (:option*
   transient-history-limit 30
   transient-highlight-mismatched-keys  t
   transient-align-variable-pitch       t
   transient-force-fixed-pitch          t
   transient-detect-key-conflicts       t)
  (:with-map transient-map
    (:bind
     "M-v" transient-scroll-down
		 "<escape>" transient-quit-one))
	(:when-loaded
		(transient-define-prefix transient-map-toggle ()
			"Transient map for toggle mode"
			[["EDITOR"
				("w" "write mode" olivetti-mode)
				("i" "imenu list" imenu-list)
				;; ("q" "quickrun" quickrun-autorun-mode)
				("t" "topsy" topsy-mode)]
			 ["EMACS"
				("cl" "command log" global-command-log-mode)]
			 ["PARA"
				("pd" "personal DB"
				 (lambda () (interactive)
					 (org-open-file
						(expand-file-name deku/note-para-file deku/note-dir))))]])
		(:global
		 "C-c t" transient-map-toggle)))



(provide 'init-transient)
;;; init-transient.el ends here
