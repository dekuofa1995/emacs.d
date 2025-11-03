;;; init-icons.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup nerd-icons
  (:require nerd-icons)
  (:when-loaded
    ;; fix orig. nerd dashboard oct icon missing
    (let ((icons nerd-icons-mode-icon-alist))
      (setq nerd-icons-mode-icon-alist
            (cons '(benchmark-init/tree-mode nerd-icons-codicon "nf-cod-dashboard"
                                             :face nerd-icons-blue)
                  (delq (assq 'benchmark-init/tree-mode icons) icons))))))

(setup nerd-icons-dired
  (:hooks dired-mode-hook nerd-icons-dired-mode))

(setup nerd-icons-ibuffer
  (:hooks ibuffer-mode-hook nerd-icons-ibuffer-mode))

(setup nerd-icons-corfu
	(:load-after corfu)
	(:when-loaded
		(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(setup kind-icon
	(:after company)
	(:option* kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8
																							 :scale 1.0 :background nil))
	(let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
				 (formatter (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
		(defun my-company-kind-icon-margin (cand _selected)
			(funcall formatter cand))
		(:hooks company-mode-hook
						(lambda ()
							(setq company-format-margin-function #'my-company-kind-icon-margin)))))

(provide 'init-icons)
;;; init-icons.el ends here
