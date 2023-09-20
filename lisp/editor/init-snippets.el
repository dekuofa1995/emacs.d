;;; init-snippets.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup tempel
	(:doc "Simple templates for emacs.")
	(:url "https://github.com/minad/tempel")
	(:tag "snippet")
	(:once (list :hooks 'prog-mode-hook :packages 'consult)
	  (require 'tempel))
	(:option*
	 tempel-trigger-prefix "<")
	(:global
	 "C-M-i" tempel-expband
	 "M-*"   tempel-insert)
	(:with-map tempel-map
	  (:bind
	   "C-p"   tempel-previous
	   "C-n"   tempel-next))
	(:doc " Add the Tempel Capf to `completion-at-point-functions'.
`tempel-expand' only triggers on exact matches. Alternatively use
`tempel-complete' if you want to see all matches, but then you
should also configure `tempel-trigger-prefix', such that Tempel
does not trigger too often when you don't expect it. NOTE: We add
`tempel-expand' *before* the main programming mode Capf, such
that it will be tried first.")
	(:when-loaded
	  (defun tempel-setup-capf ()
	    (setq-local completion-at-point-functions
									(add-to-list 'completion-at-point-functions #'tempel-complete)))
	  (add-hook 'prog-mode-hook #'tempel-setup-capf)
	  (add-hook 'org-mode-hook #'tempel-setup-capf)
	  (defun tempel-reload ()
	    "Force reload tempel's templates."
	    (interactive)
	    (setq tempel--path-templates nil))))

(provide 'init-snippets)
;;; init-snippets.el ends here
