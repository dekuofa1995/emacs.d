;;; init-roam.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defun +switch-roam-repo (&optional selected)
	(interactive)
	;; TODO: update bibtex, ebib, citar's path
	(let* ((note-repos deku/roam-repos)
				 (selected (or selected
											 (completing-read "Select from notes: " note-repos nil t nil nil deku/roam-active-repo)))
				 (repo (-> (assoc selected note-repos)
									 cdr))
				 (path (expand-file-name (plist-get repo :path)))
				 (db (or  (plist-get repo :db) (expand-file-name "roam.db" path))))
		(customize-save-variable 'deku/roam-active-repo selected)
		(message "Switch Roam Repo to: %s" selected)
		(setq org-roam-db-location db
					org-roam-directory path)))

(defun +roam-menu-title ()
	(format  "Current Note Repo: %s" deku/roam-active-repo))
(defun +switch-personal-repo ()
	(interactive)
	(+switch-roam-repo "personal"))
(defun +switch-business-repo ()
	(interactive)
	(+switch-roam-repo "business"))


(setup org-roam
	(:load+ org-roam)
	(let ((deku/roam-dir (get-roam-dir)))
		(:option*
		 org-roam-directory deku/roam-dir
		 org-roam-database-connector 'sqlite-builtin
		 org-roam-db-location (expand-file-name "roam.db" deku/roam-dir)
		 org-roam-db-gc-threshold most-positive-fixnum
		 org-roam-completion-everywhere t
		 org-roam-capture-templates
		 '(("c" "card" plain
				"%?"
				:if-new (file+head "cards/${slug}.org"
													 "#+title: ${title}\n")
				:immediate-finish t
				:unnarrowed t)
			 ("r" "reference" plain "%?"
				:if-new
				(file+head "references/${title}.org" "#+title: ${title}\n")
				:immediate-finish t
				:unnarrowed t)
			 ("a" "article" plain "%?"
				:if-new
				(file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
				:immediate-finish t
				:unnarrowed t))
		 org-roam-node-display-template
		 (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))
	(:when-loaded
		(:require emacsql-sqlite-builtin
							org-roam-db
							org-roam-compat)
		(org-roam-db-autosync-enable)
		(cl-defmethod org-roam-node-type ((node org-roam-node))
			"Return the TYPE of NODE."
			(condition-case nil
					(file-name-nondirectory
					 (directory-file-name
						(file-name-directory
						 (file-relative-name (org-roam-node-file node) org-roam-directory))))
				(error ""))))
	(:after simple-httpd
		(defservlet* notes/:id text/plain ()
			"Servlet for accessing node content."
			(insert (org-roam-ui--get-text (org-link-decode id)))
			(httpd-send-header t "text/plain" 200 :Access-Control-Allow-Origin "*"))

		(defservlet* images/:file text/plain ()
			"Servlet for accessing images found in org-roam files."
			(progn
				(httpd-send-file t (org-link-decode (f-join org-roam-directory "images" file) ))
				(httpd-send-header t "text/plain" 200 :Access-Control-Allow-Origin "*"))))
	(:after transient
		(require 'dash)
		(transient-define-prefix transient-map-roam ()
			"Org Roam."
			[:description +roam-menu-title
										["CAPTURE"
										 ("c" "capture"  org-roam-capture)
										 ("tc" "today cap"  org-roam-dailies-capture-today)
										 "EDIT"
										 ("i" "insert" org-roam-node-insert)
										 ("b" "buffer" org-roam-buffer-display-dedicated)
										 ("ra" "remove alias" org-roam-alias-remove :transient t)
										 ("rt" "remove tags" org-roam-tag-remove :transient t)
										 ("at" "add tags" org-roam-tag-add :transient t)
										 ("aa" "add alias" org-roam-alias-add :transient t)
										 ("rf" "refile"   org-roam-refile)]
										["SHOW"
										 "Repos"
										 ("sp" "personal repo" +switch-personal-repo :transient t)
										 ("sb" "business repo" +switch-business-repo :transient t)
										 "🢆 Find"
										 ("fr" "find ref" org-roam-ref-find)
										 ("fn" "find node" org-roam-node-find)
										 ("on" "open node" org-roam-node-open)
										 ("gv" "grep visit" org-roam-grep-visit)
										 ("nv" "node visit" org-roam-node-visit)
										 ("gh" "graph" org-roam-graph)
										 "🢆 TIMED"
										 ("gt" "goto today" org-roam-dailies-goto-today :transient t)
										 ("gn" "goto next" org-roam-dailies-goto-previous-note :transient t)
										 ("gp" "goto prev" org-roam-dailies-goto-previous-note :transient t)]
										["DB"
										 ("S" "sync"    org-roam-db-sync)
										 ;; ("S" "setup" org-roam-db-autosync-enable)
										 ("DA" "diagnose" org-roam-diagnostics)
										 ("Dn" "diagnose" org-roam-db-diagnose-node)]])
		(:global
		 ["C-c r"] transient-map-roam))
	(:with-map org-roam-mode-map

		(:bind "C-x u" org-move-subtree-up
					 "C-x d" org-move-subtree-down)))



(provide 'init-roam)
;;; init-roam.el ends here
