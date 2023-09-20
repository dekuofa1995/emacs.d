;;; init-themes.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup kaolin-themes)

(setup ef-themes)

(setup doom-themes
  (:when-loaded
    (doom-themes-org-config)))

(provide 'init-themes)
;;; init-themes.el ends here
