;;; init-project.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup projectile
	(:when-loaded
		(projectile-mode t))
  (:option*
	 projectile-enable-caching 'persistent
	 projectile-project-search-path '("~/git/project")
   ;; why choose hybrid https://emacs-china.org/t/projectile/17319/10
   projectile-indexing-method 'hybrid
   projectile-require-project-root t) ;; only enable find file command in project
  (:global
   "s-p"  projectile-command-map
   [remap project-switch-project] projectile-switch-project)
	(:after transient
		(:autoload consult-todo-dir)
		;; Take from consult-todo.el
		(defun deku/consult-todo-projectile ()
			"Jump to hl-todo keywords in current project."
			(interactive)
			(consult-todo-dir
			 (when-let* ((project (projectile-project-root)))
				 project)))

		(transient-define-prefix deku/trans-projectile-map ()
			"Projectile command map."
			["Transient menu for projectile commands."
			 ["Command"
				("c p" "package project" projectile-package-project)
				("c t" "test project" projectile-test-project)
				("c r" "run project cmd" projectile-run-project)
				("c i" "install project" projectile-install-project)
				("c c" "compile project" projectile-compile-project)
				("!" "run shell cmd in root" projectile-run-shell-command-in-root)
				("&" "(async)run shell cmd in root" projectile-run-async-shell-command-in-root)
				("x 4 v" "run vterm in other window" projectile-run-vterm-other-window)
				("x v" "run vterm" projectile-run-vterm)
				("x g" "run gdb" projectile-run-gdb)]
			 ["Buffer"
				("B" "show project buffers" projectile-display-buffer)
				("l" "ibuffer" projectile-ibuffer)
				("S" "save buffers" projectile-save-project-buffers)
				("K" "kill buffers" projectile-kill-buffers)
				("b" "switch project buffer" projectile-switch-to-buffer)]
			 ["File"
				("o f" "find file in other window" projectile-find-file-other-window)
				("f" "find file" projectile-find-file)
				("g" "find file dwim" projectile-find-file-dwim)
				("t f" "find test files in project" projectile-find-test-file)
				("t t" "toggle implementation and test" projectile-toggle-between-implementation-and-test)]
			 ["Search"
				("s g" "grep" projectile-grep)
				("s r" "ripgrep" projectile-ripgrep)
				("s x" "references" projectile-find-references)
				("j" "jump to tag" projectile-find-tag)
				("s t" "consult todo" deku/consult-todo-projectile)]
			 ["Edit"
				("e r" "replace" projectile-replace)
				("e R" "regex replace"  projectile-replace-regexp)]
			 ["Directory"
				("d" "find dir " projectile-find-dir)
				("o d" "find dir in other window" projectile-find-dir-other-window)
				("D" "dired" projectile-dired)
				("o D" "dired in other window" projectile-dired-other-window)]
			 ["Projectile"
				("p" "switch project" projectile-switch-project)
				("q" "switch open project" projectile-switch-open-project)
				("i" "invalidate cache" projectile-invalidate-cache :transient t)
				("z" "cache current file" projectile-cache-current-file)]])
		(:global
		 "s-p" deku/trans-projectile-map)))

(provide 'init-project)
;;; init-project.el ends here
