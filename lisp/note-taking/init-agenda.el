;;; init-agenda.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setq org-agenda-files (list "inbox.org" "agenda.org" "projects.org"))
(define-key global-map (kbd "C-c a") #'org-agenda)
;; remove the redundant tags
(setq org-agenda-hide-tags-regexp ".")
;; include entries from Emacs diary into agenda
;; org-agenda-include-diary    t
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))
;; (define-key org-mode-map (kbd "C-'") nil) ;; orig. org-cycle-agenda-files
;; conflect with avy-goto-char-2

(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "FIXED(f)" "HOLD(h)" "HACK(H)" "|" "DONE(d)")))

(setq org-gtd-update-ack "3.0.0")
(setup org-gtd
  (:once (list :packages 'org)
    (require 'org-gtd))
  (:option org-gtd-update-ack "3.0.0")
  (:option*
   org-gtd-directory "~/Notes/GTD"
	 org-gtd-areas-of-focus '("Home" "Health" "Career" "Learn")
   org-edna-use-inheritance t
   org-gtd-organize-hooks '(org-gtd-set-area-of-focus org-set-tags-command))
  (:global
   "C-c d c"  org-gtd-capture
   "C-c d e"  org-gtd-engage
   "C-c d p"  org-gtd-process-inbox
	 "C-c d n" org-gtd-show-all-next
	 "C-c d s" org-gtd-review-stuck-projects)
	(:with-map org-gtd-clarify-map
		(:bind "C-c c" org-gtd-organize))
  (:when-loaded
    (org-edna-mode)))

(provide 'init-agenda)
;;; init-agenda.el ends here
