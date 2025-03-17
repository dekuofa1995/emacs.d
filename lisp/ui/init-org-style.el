;;; init-org-style.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup org-modern
	(:option* org-modern-todo-faces
						(let ((weight 'bold)
									(color "white"))
							`(("TODO" :background "#cc9393" :foreground ,color :weight ,weight)
								("DOING" :background "#7cb8bb" :foreground ,color :weight ,weight)
								("WAITING" :background "#d0bf8f" :foreground ,color :weight ,weight)
								("CANCEL" :background "#8c5353" :foreground ,color :weight ,weight)
								("DONE" :background "#afd8af" :foreground ,color :weight ,weight))))
  (:hooks org-mode-hook org-modern-mode)
  (:hooks org-agenda-finalize-hook org-modern-agenda))

(setup org
	(:doc "customize org style")
	(:also-load org-modern)
	(:option
	 org-todo-keywords
   '((sequence "TODO(t)" "DOING(d)" "NEXT(n)" "FIXED(f)" "WAITING(w)" "CANCEL(c)" "HACK(H)" "|" "DONE(D)"))
	 org-todo-keyword-faces '(("TODO"    . warning)
                            ("DOING"   . success)
                            ("WAITING" . error)
                            ("VERIFY"  . error)
                            ("DONE"    . shadow)
                            ("CANCEL"  . shadow))
	 org-hide-emphasis-markers t)
	(:when-loaded
		(add-to-list 'org-emphasis-alist
								 '("=" (:box (:line-width -2 :color "gray50" :style released-button) :inherit org-verbatim)))))

(setup org-sticky-header
  (:hooks org-mode-hook org-sticky-header-mode))

(provide 'init-org-style)
;;; init-org-style.el ends here
