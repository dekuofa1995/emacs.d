;;; init-rust.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup rust-mode
  (:hooks
   rust-mode-hook eglot-ensure)
  (:when-loaded
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))))

(provide 'init-rust)
;;; init-rust.el ends here
