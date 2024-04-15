;;; init-roam.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:


(let ((deku/roam-dir (get-roam-dir)))
	(setup org-roam
		(:load+ org-roam)
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
		 (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
		(:when-loaded
			(:require emacsql-sqlite-builtin)
			(require 'org-roam-db)
			(require 'org-roam-compat)
			(org-roam-db-autosync-enable)
			(cl-defmethod org-roam-node-type ((node org-roam-node))
				"Return the TYPE of NODE."
				(condition-case nil
						(file-name-nondirectory
						 (directory-file-name
							(file-name-directory
							 (file-relative-name (org-roam-node-file node) org-roam-directory))))
					(error ""))))
		(:after transient
			(require 'dash)
			(defun +switch-roam-repo ()
				(interactive)
				;; TODO: update bibtex, ebib, citar's path
				(let* ((note-repos deku/roam-repos))
					(let* ((selected (completing-read "Select from notes: " note-repos nil t nil nil deku/roam-active-repo))
								 (repo (-> (assoc selected note-repos)
													 cdr))
								 (path (expand-file-name (plist-get repo :path)))
								 (db (or  (plist-get repo :db) (expand-file-name "roam.db" path))))
						(setq deku/roam-active-repo selected
									org-roam-db-location db
									org-roam-directory path))))
			(defun +roam-menu-title ()
				(format  "Current Note Repo: %s" deku/roam-active-repo))
			(transient-define-prefix transient-map-roam ()
				"Org Roam."
				[:description +roam-menu-title
											["CAPTURE"
											 ("c" "capture"  org-roam-capture)
											 ("tc" "today cap"  org-roam-dailies-capture-today)
											 "EDIT"
											 ("i" "insert" org-roam-node-insert
												"rf" "refile"   org-roam-refile
												"b" "buffer" org-roam-buffer-display-dedicated
												"aa" "add alias" org-roam-alias-add :transient t
												"ra" "remove alias" org-roam-alias-remove :transient t
												"rt" "remove tags" org-roam-tag-remove :transient t
												"at" "add tags" org-roam-tag-add :transient t)]
											["SHOW"
											 "Repos"
											 ("S" "switch repo" +switch-roam-repo :transient t)
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
											 ("s" "sync"    org-roam-db-sync)
											 ;; ("S" "setup" org-roam-db-autosync-enable)
											 ("DA" "diagnose" org-roam-diagnostics)
											 ("Dn" "diagnose" org-roam-db-diagnose-node)]])
			(:global
			 [f3] transient-map-roam))
		(:with-map org-roam-mode-map
			(:bind "C-x u" org-move-subtree-up
						 "C-x d" org-move-subtree-down))))



(provide 'init-roam)
;;; init-roam.el ends here
