;;; init-lsp.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup eglot
  (:option*
   eglot-ignored-server-capabilites '(:documentHighlightProvider)
   read-process-output-max (* 256 1024)
	 eglot-events-buffer-size 5000)
  (:global
   "C-M-r"	eglot-rename
   "<C-return>" eglot-code-actions
   ;; "C-M-f"	eglot-code-action-quickfix
   "C-c C-l"	eglot-code-action-line
   "C-c C-e"	eglot-code-action-extract
   "C-c C-f"	eglot-format
   "C-c C-o"	eglot-code-action-organize-imports
   "C-c C-h"  eldoc))

(defun deku/eglot-auto-format ()
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format)))

(provide 'init-lsp)
;;; init-lsp.el ends here
