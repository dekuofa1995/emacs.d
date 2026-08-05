;;; init-r.el -- Deku Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup ess
  (:once (list :hooks 'ess-r-mode-hook)
    (define-key ess-mode-map (kbd "C-c f") #'eglot-format))
	(:option* ess-style 'RStudio)
  (:hooks 'ess-r-mode-hook deku/eglot-auto-format
					'ess-r-mode-hook eglot-ensure))

(provide 'init-r)
;;; init-r.el ends here
