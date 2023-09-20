;;; init-elisp.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup ob-emacs-lisp
  (:once (list :files 'org)
    (require 'ob-emacs-lisp))
  (:autoload org-babel-execute:emacs-lisp
    org-babel-expand-body:emacs-lisp))

(provide 'init-elisp)
;;; init-elisp.el ends here
