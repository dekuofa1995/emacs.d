;;; init-embark.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup embark
	(:load-after vertico)
	(:autoload embark--targets)
  (:global
   "C-."   embark-act
   "C-h B" embark-bindings ;; alternative for `describe-bindings'
   "C-M-'" embark-dwim)
	(:option* embark-indicators '(embark-minimal-indicator  ; default is embark-mixed-indicator
																embark-highlight-indicator
																embark-isearch-highlight-indicator)
						;; replace which-key
						prefix-help-command #'embark-prefix-help-command)
  (:when-loaded
		(:also-load posframe)
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
								 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))))

(setup embark-consult
  (:once (list :packages 'embark)
    (require 'embark-consult))
  (:comment (:hooks
						 embark-collect-mode-hook consult-preview-at-point-mode)))

(provide 'init-embark)
;;; init-embark.el ends here
