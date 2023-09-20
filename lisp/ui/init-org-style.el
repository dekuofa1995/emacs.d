;;; init-org-style.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup org-modern
  (:hooks org-mode-hook org-modern-mode)
  (:hooks org-agenda-finalize-hook org-modern-agenda))

(setq org-todo-keyword-faces '(("TODO"    . warning)
                               ("DOING"   . success)
                               ("WAITING" . error)
                               ("VERIFY"  . error)
                               ("DONE"    . shadow)
                               ("CANCEL"  . shadow)))

(setq org-hide-emphasis-markers t)

(with-eval-after-load "org"
  (add-to-list 'org-emphasis-alist
							 '("=" (:box (:line-width -2 :color "gray50" :style released-button) :inherit org-verbatim))))

(setup org-sticky-header
  (:hooks org-mode-hook org-sticky-header-mode))

(provide 'init-org-style)
;;; init-org-style.el ends here
