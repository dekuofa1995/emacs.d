;;; init-shell.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup fish-mode
  (:init
      (add-to-list 'auto-mode-alist
		'("\\.fish\\'" . fish-mode))))

(provide 'init-shell)
;;; init-shell.el ends here
