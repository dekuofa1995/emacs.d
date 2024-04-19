;;; init-formatter.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup apheleia
  (:global
   "C-c C-f" apheleia-format-buffer)
  (:hooks prog-mode-hook apheleia-mode)
	(:when-loaded
		(global-auto-revert-mode t))
  (:doc "python format config")
  (:when-loaded
    (setf (alist-get 'python-ts-mode apheleia-mode-alist)
          'ruff)
    (setf (alist-get 'python-mode apheleia-mode-alist)
          'ruff))
  (:doc "Clojure format config")
  (:when-loaded
    (push '(cljfmt . ("zprint" ;; "'{:width 120}'" set options at here
											"-w" filepath))
					apheleia-formatters)
    (push '(clojure-mode . cljfmt) apheleia-mode-alist)))

(provide 'init-formatter)
;;; init-formatter.el ends here
