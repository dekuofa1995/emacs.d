;;; init-org-mode.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:


(defun org-to-docx (&optional file)
	(interactive)
	(let* ((file (or file (buffer-file-name)))
				 (fname (file-name-sans-extension file))
				 (cmd (format "pandoc -o %s.docx %s" fname file)))
		(async-shell-command cmd)))

(setup org
	(:autoload org-yank org-do-promote org-do-demote
						 org-promote-subtree org-demote-subtree org-toggle-heading
						 org-sort visible-mode org-narrow-to-block
						 org-narrow-to-subtree org-narrow-to-element
						 org-mark-element org-mark-subtree
						 org-set-tags)
	(:hooks org-mode-hook visual-line-mode
					org-mode-hook (lambda () (setq-local scroll-margin 0)))
	(:load+ org)
	(:also-load org-ql)
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
		(:option* org-directory "~/Notes/org"
							org-tag-re "[[:alnum:]_-@#%]+"
							org-toggle-pretty-entities t
							org-auto-align-tags nil
							org-tags-columns 0
							org-catch-invisible-edits 'show-and-error
							org-startup-with-inline-images t
							org-special-ctrl-a/e t
							org-hide-emphasis t
							org-capture-templates capture-templates
							org-pretty-entities t))
	(:global-bind "C-c n c" org-capture)
	(:with-map org-mode-map
		(:bind
		 "C-c C-o"        transient-map-org
		 "C-c o"      org-open-at-point
		 "M-<right>"    org-do-demote
		 "M-<left>"     org-do-promote
		 "M-S-<right>"  org-demote-subtree
		 "M-S-<left>"   org-promote-subtree
		 "M-<up>"       org-move-subtree-up
		 "M-<down>"     org-move-subtree-down
		 "C-S-<return>"  org-insert-subheading
		 ;; refile: move content to better localtion/file
		 "C-y"          org-yank)
		(:unbind "C-'" "C-,") ;; org-cycle-agenda-files
		)
	(:after transient
		(defun deku/org-todo-view ()
			(interactive)
			(org-ql-search (current-buffer)
				'(and (priority >= "B")
							(todo))))
		(defun deku/org-todo-query ()
			(interactive)
			(org-ql-query
				:select 'element-with-markers
				:from (current-buffer)
				:where '(and (priority >= "B") (todo))))
		(transient-define-prefix transient-map-org ()
			"ORG."
			[["EDIT"
				("yy" "yank"  org-yank)
				("ym" "yank media"  yank-media)
				("rf" "refine"  org-refile)
				("rr" "roam refine"  org-roam-refile)
				("<" "promote"  org-do-promote :transient t)
				(">" "demote"  org-do-demote :transient t)
				("+" "p-subtree"  org-promote-subtree :transient t)
				("-" "d-subtree"  org-demote-subtree :transient t)
				("*" "togg-heading"  org-toggle-heading :transient t)
				("^" "sort"  org-sort)
				("ta" "set tags" org-set-tags-command)]
			 ["Pandoc"
				("pd" "pandoc to docx" org-to-docx)]
			 ["VIEW"
				("v" "visible" visible-mode)
				("td" "todo view" deku/org-todo-view)
				"🢆 NARROW"
				("nw" "widen" widen)
				("nt" "subtree" org-narrow-to-subtree)
				("nb" "block" org-narrow-to-block)
				("ne" "element" org-narrow-to-element)]
			 ["Mark"
				("mt" "subtree" org-mark-subtree)
				("me" "element" org-mark-element)]]))
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
		(org-indent-mode -1)))

;; code from https://github.com/Elilif/.elemacs/blob/ff4f2e3076de5aa653479f37b77d294940d0a828/lib/lib-embark.el#L51
;; preview image while using `find-file'
(defun eli/image-preview (&rest _args)
	(let* ((target (embark--targets))
				 (file-path (plist-get (car target) :target))
				 (name (file-name-nondirectory file-path))
				 (mode (assoc-default name auto-mode-alist #'string-match)))
		(posframe-hide-all)
		(when (memq mode '(image-mode))
			(with-current-buffer (get-buffer-create "*image*")
				(setq inhibit-read-only t)
				(erase-buffer)
				(insert-file-contents file-path)
				(set-auto-mode-0 mode))
			(when (posframe-workable-p)
				(posframe-show "*image*"
											 :poshandler #'posframe-poshandler-frame-center)))))

(defun eli/select-images ()
	(interactive)
	(read-file-name "Select a file: "
									deku/images-dir))

(advice-add 'eli/select-images :before
						(lambda (&rest _args)
							(letrec ((eli/remove-preview
												(lambda ()
													(remove-hook 'post-command-hook #'eli/image-preview)
													(posframe--kill-buffer "*image*")
													(remove-hook 'minibuffer-exit-hook eli/remove-preview))))
								(add-hook 'post-command-hook #'eli/image-preview)
								(add-hook 'minibuffer-exit-hook eli/remove-preview))))

(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

;; (define-key global-map (kbd "C-c i") 'org-capture-inbox)

(defun meomacs-after-babel-execute ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'meomacs-after-babel-execute)

(setup ob-async
	(:comment
   (:once (list :hooks 'org-mode-hook)
     (require 'ob-async))
	 (:option*
		ob-async-no-async-languages-alist '("jupyter-python" "jupyter-R" "ipython"))))

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
			 (mermaid . t)
			 ;; (jupyter . t)
			 (plantuml . t)))))

(provide 'init-org-mode)
;;; init-org.el ends here
