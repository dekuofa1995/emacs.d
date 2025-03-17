;;; init-rust.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup rust-mode
	(:also-load rust-playgroud)
  (:hooks
   (rust-ts-mode-hook rust-mode-hook) eglot-ensure)
	(:file-match (rx ".rs$")))

(setup cargo-mode
	(:option*
	 compilation-scroll-output t)
	(:hooks (rust-mode-hook rust-ts-mode-hook) cargo-minor-mode))

(setup rust-playgroud)

(provide 'init-rust)
;;; init-rust.el ends here
