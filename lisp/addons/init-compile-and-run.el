;;; init-compile-and-run.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup quickrun
  (:global
   "C-x P" quickrun-keymap)
	;; (:autoload quickrun-autorun-mode)
  (:when-loaded
   (defvar quickrun-keymap
     (let ((keymap (make-keymap)))
       (define-key keymap "r"   #'quickrun)
       (define-key keymap "M-r" #'quickrun-select)
       (define-key keymap "R"   #'quickrun-region)
       (define-key keymap "a r" #'quickrun-with-arg)
       (define-key keymap "s r" #'quickrun-shell) ;; default run from eshell
       (define-key keymap "c"   #'quickrun-compile-only)
       (define-key keymap "s c" #'quickrun-compile-only-select)))
   (defalias 'quickrun-keymap quickrun-keymap)
   (global-set-key (kbd "C-x P") quickrun-keymap)))

(provide 'init-compile-and-run)
;;; init-compile-and-run.el ends here
