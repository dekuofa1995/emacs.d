;;; init-project.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

;; (setup find-file-in-project
;;   (:require find-file-in-project)
;;   (:global
;;    "C-c C-f" find-file-in-project)
;;   (:when-loaded
;;     ;; custom exclude dirs
;;     (let ((emacs
;; 	   '("straight" "elpa" "eln-cache" "\.cache" "lib" "devdocs" "epkgs"
;; 	     "autosave-list" "tree-sitter" "transient" "url" "newsticker"))
;; 	  (emacs-custom '("dirvish" "racket-mode"))
;; 	  (build '("dist")))
;;       (dolist (dlist (list emacs emacs-custom build))
;; 	(dolist (dir dlist)
;; 	  (add-to-list 'ffip-prune-patterns (format "*/%s" dir)))))))

(defun projectile-ensure (&rest _)
	(unless (bound-and-true-p projectile-mode)
		(projectile-mode)))

(setup projectile
	(:autoload projectile-switch-project)
	(:advice projectile--find-file :before #'projectile-ensure)
  (:option*
   projectile-enable-caching t
   ;; why choose hybrid https://emacs-china.org/t/projectile/17319/10
   projectile-indexing-method 'hybrid
   projectile-require-project-root t) ;; only enable find file command in project
  (:global
   "s-p"  projectile-command-map
   [remap project-switch-project] projectile-switch-project))

(provide 'init-project)
;;; init-project.el ends here
