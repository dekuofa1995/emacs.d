;;; init-doc.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup eldoc
  (:hooks (prog-mode org-mode) eldoc-mode)
	(:option* eldoc-help-at-pt nil)
	(:when-loaded
		;; 来自 https://emacs-china.org/t/elisp-eldoc/7571 eldoc 显示函数参数
		(define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
			"If SYM is a function, append its docstring."
			(concat
			 (apply orig-fun sym r)
			 (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
							(oneline (and doc (substring doc 0 (string-match "\n" doc)))))
				 (and oneline
							(concat "  |  " (propertize oneline 'face 'italic))))))))

(setup eldoc-box
	(:comment
	 (:after eldoc)
	 (:hooks eldoc-mode-hook eldoc-box-hover-mode)
	 (:option* eldoc-box-clear-with-C-g t)))

(setup dash-at-point
  (:once (list :hooks 'prog-mode-hook)
    (require 'dash-at-point))
  (:when-loaded
    (add-to-list 'dash-at-point-mode-alist '(clojure-ts-mode . "clj"))
    (add-to-list 'dash-at-point-mode-alist '(java-ts-mode . "j8")))
  (:with-map prog-mode-map
    (:bind
     "C-c C-d" dash-at-point
     "C-c H"   dash-at-point-with-docset)))

(provide 'init-doc)
;;; init-doc.el ends here
