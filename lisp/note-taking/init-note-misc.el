;;; init-note-misc.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup markdown-mode
  (:with-map markdown-mode-map
    "C-x C-v" #'markdown-toggle-markup-hiding))

(provide 'init-note-misc)
;;; init-note-misc.el ends here
