;;; init-snippets.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup tempel
	(:disable t)
	(:doc "Simple templates for emacs.")
	(:url "https://github.com/minad/tempel")
	(:tag :snippet)
	(:hooks org-mode-hook (lambda () (setq-local tempel-trigger-prefix "<")))
	(:load-after corfu)
	(:global
	 "C-c C-i" tempel-complete ;; also C-c Tab
	 "M-*"   tempel-insert)
	(:with-map tempel-map
	  (:bind
		 "C-p"   tempel-previous
		 "C-n"   tempel-next
		 "TAB"   tempel-next))
	(:doc " Add the Tempel Capf to `completion-at-point-functions'.
`tempel-expand' only triggers on exact matches. Alternatively use
`tempel-complete' if you want to see all matches, but then you
should also configure `tempel-trigger-prefix', such that Tempel
does not trigger too often when you don't expect it. NOTE: We add
`tempel-expand' *before* the main programming mode Capf, such
that it will be tried first."))

(setup yasnippet
	(:hooks (org-mode-hook prog-mode-hook) yas-minor-mode)
	;; (:option
	;;  yas-snippet-dirs `,(append yas-snippet-dirs (expand-file-name "snippets/clojure" user-emacs-directory)))
	(:when-loaded
		(yas-reload-all)))

(setup yasnippet-snippets
	(:doc "a collection of yasnippet snippets for many languages ")
	(:url "https://github.com/AndreaCrotti/yasnippet-snippets")
	(:tag :snippet))

(provide 'init-snippets)
;;; init-snippets.el ends here
