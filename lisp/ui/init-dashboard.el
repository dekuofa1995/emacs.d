;;; init-dashboard.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defun open-project-in-dired (dir)
  (dired dir))
(setup dashboard
  ;; (:require dashboard)
	(:once (list :hooks 'window-setup-hook)
		(require 'dashboard))
  (:option
   initial-buffer-choice        (lambda () (get-buffer-create "*dashboard*")))
  (:option*
   dashboard-set-navigator      t
   dashboard-display-icons-p    t
   dashboard-projects-switch-function #'open-project-in-dired
   dashboard-icon-type          'nerd-icons
   dashboard-set-file-icons     t
   dashboard-set-heading-icons  t
   dashboard-center-content     t
   dashboard-startup-banner     (expand-file-name "spacemacs-logo.png" user-emacs-directory)
   dashboard-banner-logo-title  "Have a Nice Day!"
   dashboard-items             '((recents  . 10) (projects . 10))
	 dashboard-heading-icons '((recents . "nf-oct-clock")
														 (bookmarks . "nf-oct-bookmark")
														 (agenda . "nf-oct-calendar")
														 (projects . "nf-oct-rocket")
														 (registers . "nf-oct-database")))

  (:with-map dashboard-mode-map
    (:bind
     "C-c p" project-find-file
     "n"     dashboard-next-line
     "e"     dashboard-previous-line
     "g"     dashboard-open
     "o"     dashboard-return))
  (:when-loaded
    ;; https://github.com/emacs-dashboard/emacs-dashboard/issues/471
    ;; (advice-add #'dashboard-replace-displayable :override #'identity)
    (dashboard-setup-startup-hook)))

(setup recentf
  (:when-loaded
    (let ((ignore-files '("\\.cache" "bookmark" "agenda.org" "inbox.org" "projects.org" ".emacs.d")))
      (dolist (file ignore-files) (add-to-list 'recentf-exclude file)))  ))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
