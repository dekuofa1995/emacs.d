;;; init-project.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup find-file-in-project
  (:require find-file-in-project)
  (:global
   "C-c C-f" find-file-in-project)
  (:when-loaded
    ;; custom exclude dirs
    (let ((emacs
	   '("straight" "elpa" "eln-cache" "\.cache" "lib" "devdocs" "epkgs"
	     "autosave-list" "tree-sitter" "transient" "url" "newsticker"))
	  (emacs-custom '("dirvish" "racket-mode"))
	  (build '("dist")))
      (dolist (dlist (list emacs emacs-custom build))
	(dolist (dir dlist)
	  (add-to-list 'ffip-prune-patterns (format "*/%s" dir)))))))

(setup projectile
  (:require projectile)
  (:hooks dired-mode-hook projectile-mode)
  (:option*
   projectile-enable-caching t
   ;; why choose hybrid https://emacs-china.org/t/projectile/17319/10
   projectile-indexing-method 'hybrid
   projectile-require-project-root t) ;; only enable find file command in project
  (:with-map projectile-mode-map
    (:bind "s-p"  projectile-command-map
           [remap project-switch-project] projectile-switch-project)))

(provide 'init-project)
;;; init-project.el ends here
