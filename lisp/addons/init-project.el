;;; init-project.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup projectile
	(:when-loaded
		(projectile-mode t))
  (:option*
	 projectile-project-search-path '("~/git/project")
   projectile-enable-caching t
   ;; why choose hybrid https://emacs-china.org/t/projectile/17319/10
   projectile-indexing-method 'hybrid
   projectile-require-project-root t) ;; only enable find file command in project
  (:global
   "s-p"  projectile-command-map
   [remap project-switch-project] projectile-switch-project))

(provide 'init-project)
;;; init-project.el ends here
