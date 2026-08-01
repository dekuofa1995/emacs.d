;;; init-telega.el --- summary -*- lexical-binding: t -*-

;; Author: Mob
;; Maintainer: Mob
;; Version: 1.0

;;; Code:

(setup telega
	(:doc "https://zevlg.github.io/telega.el/")
	(:option*
	 telega-server-libs-prefix "/opt/homebrew/opt/tdlib"
	 telega-avatar-workaround-gaps-for '(return t))
	(define-key global-map (kbd "C-c t") telega-prefix-map))

(provide 'init-telega)

;;; init-telega.el ends here
