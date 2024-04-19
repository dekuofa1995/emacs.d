;;; init-latex.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:


(setup org-latex-preview
	(:after org
		(:option org-format-latex-options (plist-put org-format-latex-options :scale 4.0)))
  (:once (list :hooks'org-mode-hook)
    (require 'org-latex-preview))
  (:hooks org-mode-hook org-latex-preview-auto-mode)
  (:with-map
      (:bind "C-c C-x SPC" org-latex-preview-clear-cache))
  (:when-loaded
    (add-hook 'org-latex-preview-auto-blacklist 'iscroll-next-line)
    (add-hook 'org-latex-preview-auto-blacklist 'iscroll-previous-line)
    (setq-default
     ;; org-format-latex-options
     ;; (progn (plist-put org-format-latex-options :background "Transparent")
     ;; 	  (plist-put org-format-latex-options :scale 1.0)
     ;; 	  (plist-put org-format-latex-options :zoom
     ;; 		     (+ 0.01 (/ (face-attribute 'default :height) 100.0))))
     ;; (progn (plist-put org-latex-preview-options :scale 1.0)
     ;;      (plist-put org-latex-preview-options :zoom
     ;;                 (+ 0.01 (/ (face-attribute 'default :height) 100.0))))
     org-latex-preview-debounce 1.0
     org-latex-preview-throttle 1.0
     org-latex-preview-auto-generate 'live)))

(provide 'init-latex)
;;; init-latex.el ends here
