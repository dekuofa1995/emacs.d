;;; init-org.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:
(transient-define-prefix transient-map-org ()
	"ORG."
	[["EDIT"
		("yy" "yank"  org-yank)
		("rf" "refine"  org-refile)
		("pm" "promote"  org-do-promote :transient t)
		("dm" "demote"  org-do-demote :transient t)
		("pt" "p-subtree"  org-promote-subtree :transient t)
		("dt" "d-subtree"  org-demote-subtree :transient t)
		("*" "togg-heading"  org-toggle-heading)
		("^" "sort"  org-sort)]
	 ["VIEW"
		("tv" "visible" visible-mode)
		"🢆 NARROW"
		("nw" "widen" widen)
		("nt" "subtree" org-narrow-to-subtree)
		("nb" "block" org-narrow-to-block)
		("ne" "element" org-narrow-to-element)]
	 ["Mark"
		("mt" "subtree" org-mark-subtree)
		("me" "element" org-mark-element)]])

(let ((capture-templates
       `(("i" "Inbox" entry (file "inbox.org")
					,(concat "* TODO %?\n"
                   "/Entered on/ %U"))
         ("m" "Meeting" entry (file+headline "agenda.org" "Future")
					,(concat "* %? :meeting:\n"
                   "<%<%Y-%m-%d %a %H:00>>"))
         ("n" "Note" entry (file "notes.org")
					,(concat "* Note (%a)\n"
                   "/Entered on/ %U\n" "\n" "%?")))))
	(setup org
		(:autoload org-yank org-refile org-do-promote org-do-demote
							 org-promote-subtree org-demote-subtree org-toggle-heading
							 org-sort visible-mode widen org-narrow-to-block
							 org-narrow-to-subtree org-narrow-to-element
							 org-mark-element org-mark-subtree)
		(:load+ org)
		(:option*
		 org-directory "~/Notes/org"
		 org-toggle-pretty-entities t
		 org-auto-align-tags nil
		 org-tags-columns 0
		 org-catch-invisible-edits 'show-and-error
		 org-special-ctrl-a/e t
		 org-hide-emphasis t
		 org-capture-templates capture-templates
		 org-pretty-entities t)
		(:global "C-c n c" org-capture)
		(:with-map org-mode-map
			(:bind
			 "C-c o"        transient-map-org
			 "C-c C-o"      org-open-at-point
			 "M-<right>"    org-do-demote
			 "M-<left>"     org-do-promote
			 "M-S-<right>"  org-demote-subtree
			 "M-S-<left>"   org-promote-subtree
			 "M-<up>"       org-move-subtree-up
			 "M-<down>"     org-move-subtree-down
			 ;; refile: move content to better localtion/file
			 "C-y"          org-yank)
			(:unbind "C-'"))
		(:option*
		 org-ellipsis                        "  " ;; folding symbol
		 ;; org-startup-indented                t ;; disable for org-modern-mode's block fringe
		 org-hide-emphasis-markers           t
		 org-fontify-done-headline           t
		 org-fontify-whole-heading-line      t
		 org-fontify-quote-and-verse-blocks  t
		 org-src-tab-acts-natively           t
		 org-confirm-babel-evaluate          nil)
		(:when-loaded
			(org-indent-mode -1))))

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
  (:once (list :hooks 'org-mode-hook)
    (require 'ob-async))
	(:option*
	 ob-async-no-async-languages-alist '("jupyter-python" "jupyter-R")))

(setup org-appear
	(:once (list :files 'org)
		(require 'org-appear))
	(:hooks org-mode-hook org-appear-mode)
	(:option* org-appear-autosubmarkers t
						org-appear-keywords t
						org-appear-autoliks t))

(setup ob
	(:when-loaded
		(org-babel-do-load-languages
		 'org-babel-load-languages
		 '((emacs-lisp . t)
			 (python . t)
			 (R . t)
			 ;; (jupyter . t)
			 (plantuml . t)))))

(provide 'init-org)
;;; init-org.el ends here
