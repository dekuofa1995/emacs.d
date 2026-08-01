;;; init-lsp.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defun my/eglot-handle-request-remove-metadata (args)
  "Remove :metadata and its value from workspace/applyEdit arguments."
  (if (and (eq (nth 1 args) 'workspace/applyEdit)
           (memq :metadata args))
      (let ((new-args (list (nth 0 args) (nth 1 args))) ; server & method
            (rest (nthcdr 2 args)))                     ; remaining plist
        (while rest
          (if (eq (car rest) :metadata)
              (setq rest (cdr (cdr rest)))  ; skip key and its value
            (setq new-args (append new-args (list (car rest))))
            (setq rest (cdr rest))))
        new-args)
    args))

(setup eglot
	(:require eglot)
  (:option*
   eglot-ignored-server-capabilites '(:documentHighlightProvider)
   read-process-output-max (* 256 1024)
	 eglot-events-buffer-size 5000)
	(:with-map eglot-mode-map
	  (:bind
		 "C-M-r"	eglot-rename
		 "<C-return>" eglot-code-actions
		 ;; "C-M-f"	eglot-code-action-quickfix
		 "C-c C-l"	eglot-code-action-line
		 "C-c C-e"	eglot-code-action-extract
		 "C-c C-f"	eglot-format
		 "C-c C-o"	eglot-code-action-organize-imports
		 "C-c C-h"  eldoc))
	(:when-loaded

		(advice-add 'eglot-handle-request :filter-args
								#'my/eglot-handle-request-remove-metadata)

		))

(defun deku/eglot-auto-format ()
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format)))

(provide 'init-lsp)
;;; init-lsp.el ends here
