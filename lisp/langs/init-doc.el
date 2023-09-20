;;; init-doc.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup eldoc
  (:hooks (prog-mode org-mode) eldoc-mode))

(setup dash-at-point
  (:once (list :hooks 'prog-mode-hook)
    (require 'dash-at-point))
  (:when-loaded
    (add-to-list 'dash-at-point-mode-alist '(clojure-ts-mode . "clj"))
    (add-to-list 'dash-at-point-mode-alist '(java-ts-mode . "j8")))
  (:with-map prog-mode-map
    (:bind
     "C-c C-d" dash-at-point
     "C-c H"   dash-at-point-with-docset)))

(provide 'init-doc)
;;; init-doc.el ends here
