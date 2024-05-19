;;; init-typst.el -- typst config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup tinymist
	(:hooks typst-ts-mode-hook eglot-ensure)
  (:after* typst-ts-mode
		(require 'eglot)
		(add-to-list 'eglot-server-programs
								 '((typst-mode typst-ts-mode) . ("tinymist"))))
	(add-to-list 'auto-mode-alist '("\\.typ$" . typst-ts-mode)))

(setup typst-ts-mode
	(:option*
	 ;; typst-ts-mode-watch-options "--open"
	 typst-ts-mode-enable-raw-blocks-highlight t
	 typst-ts-mode-highlight-raw-blocks-at-startup t))

;; (defvar tip--server-process
;; 	nil
;; 	"Tip server process's object.")

;; (defun tip--start-server (buf port)
;; 	(setq tip-server-process
;; 				(start-file-process-shell-command
;; 				 "tip-server"
;; 				 buf
;; 				 (format "bin/python main.py --port=%s" port))))

;; (defun tip-ensure (&optional force)
;; 	(interactive)
;; 	(let ((port tip-server-port)
;; 				(dir (file-truename tip-server-basedir))
;; 				(process tip--server-process)
;; 				(buf (get-buffer-create "*tip-server-log*")))
;; 		(with-current-buffer buf
;; 			(setq-local default-directory dir)
;; 			(cond ((and process force)
;; 						 (message "kill and start a new tip-server.")
;; 						 (kill-process process)
;; 						 (tip--start-server buf port))
;; 						((and process (not force))
;; 						 (message "tip-server alreadly started!"))
;; 						(t
;; 						 (tip--start-server buf port))))))

(setup tip
	(:once '(:hooks typst-ts-mode-hook)
		(require 'tip))
	(:hooks typst-ts-mode-hook tip-mode
					tip-mode-hook tip-ensure)
	(:option*
	 tip-server-port 8111
	 tip-server-basedir (file-truename "~/git/tools/tip-server-py/")))

(provide 'init-typst)
;;; init-typst.el ends here
