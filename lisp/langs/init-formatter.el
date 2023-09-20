;;; init-formatter.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup apheleia
  (:global
   "C-c f" apheleia-format-buffer)
  (:hooks prog-mode-hook apheleia-mode)
  (:doc "python format config")
  (:when-loaded
    (setf (alist-get 'python-ts-mode apheleia-mode-alist)
          '(isort black)))
  (:doc "Clojure format config")
  (:when-loaded
    (push '(cljfmt . ("zprint" ;; "'{:width 120}'" set options at here
		      "-w" filepath))
	  apheleia-formatters)
    (push '(clojure-mode . cljfmt) apheleia-mode-alist)))

(provide 'init-formatter)
;;; init-formatter.el ends here
