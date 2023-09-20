;;; init-org.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup org
  (:option*
   org-directory "~/Notes/org"
   org-toggle-pretty-entities t
   org-auto-align-tags nil
   org-tags-columns 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-hide-emphasis t
   org-pretty-entities t)
  (:global "C-c n c" org-capture)
  (:with-map org-mode-map
    (:bind
     "C-c C-o"      org-open-at-point
     "M-<right>"    org-do-demote
     "M-<left>"     org-do-promote
     "M-S-<right>"  org-demote-subtree
     "M-S-<left>"   org-promote-subtree
     "M-<up>"       org-move-subtree-up
     "M-<down>"     org-move-subtree-down
     "C-c @"        org-mark-subtree
     ;; refile: move content to better localtion/file
     "C-c C-w"      org-refile
     "C-y"          org-yank
     "C-c *"        org-toggle-heading
     "C-c ^"        org-sort
     ;; narrow and widen
     "C-x n s"      org-narrow-to-subtree
     "C-x n b"      org-narrow-to-block
     "C-x n w"      widen
     "C-x C-v"      visible-mode)
    (:unbind "C-'"))
  (:option*
     org-ellipsis                        "  " ;; folding symbol
     ;; org-startup-indented                t ;; disable for org-modern-mode's block fringe
     org-hide-emphasis-markers           t
     org-fontify-done-headline           t
     org-fontify-whole-heading-line      t
     org-fontify-quote-and-verse-blocks  t
     org-src-tab-acts-natively           t)
  (:when-loaded
    (org-indent-mode -1)))

(setq org-capture-templates
      `(("i" "Inbox" entry (file "inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("m" "Meeting" entry (file+headline "agenda.org" "Future")
         ,(concat "* %? :meeting:\n"
                  "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry (file "notes.org")
         ,(concat "* Note (%a)\n"
                  "/Entered on/ %U\n" "\n" "%?"))
        ))

(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(define-key global-map (kbd "C-c i") 'org-capture-inbox)

(defun meomacs-after-babel-execute ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'meomacs-after-babel-execute)

(setup ob-async
  (:once (list :hooks org-mode-hook )
    (lambda () (require 'ob-async))))

(provide 'init-org)
;;; init-org.el ends here
