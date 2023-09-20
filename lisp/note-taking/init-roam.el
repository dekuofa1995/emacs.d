;;; init-roam.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup emacsql-sqlite-builtin )

(setup org-roam
  (:option*
   org-roam-directory (expand-file-name "~/Roam-Notes")
   org-roam-database-connector 'sqlite-builtin)
  (:when-loaded
    (defvar org-roam-keymap
      (let ((keymap (make-keymap)))
	(define-key keymap "l" 'org-roam-buffer-toggle)
	(define-key keymap "f" 'org-roam-node-find)
	(define-key keymap "g" 'org-roam-graph)
	(define-key keymap "i" 'org-roam-node-insert)
	(define-key keymap "c" 'org-roam-capture)
	(define-key keymap "s" 'org-roam-db-sync)
	keymap))
    (defalias 'org-roam-keymap org-roam-keymap)
    (global-set-key (kbd "C-c r") 'org-roam-keymap)
    (org-roam-setup)))

(provide 'init-roam)
;;; init-roam.el ends here
