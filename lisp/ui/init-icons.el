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

(provide 'init-icons)
;;; init-icons.el ends here
