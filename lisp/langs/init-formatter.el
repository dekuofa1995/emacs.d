;;; init-formatter.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup apheleia
	(:url "https://github.com/radian-software/apheleia")
  (:global-bind
   "C-c C-f" apheleia-format-buffer)
  (:hooks prog-mode-hook apheleia-mode)
	(:when-loaded
		(global-auto-revert-mode t))
  (:doc "python format config")
  (:when-loaded
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff
					(alist-get 'python-mode apheleia-mode-alist)    'ruff)
		(setf (alist-get 'clojure-mode apheleia-mode-alist)    'zprint
					(alist-get 'clojure-ts-mode apheleia-mode-alist) 'zprint))
  (:doc "Clojure format config")
  (:when-loaded
    (push '(zprint . ("zprint" "-w" filepath))
					apheleia-formatters)))

(provide 'init-formatter)
;;; init-formatter.el ends here
