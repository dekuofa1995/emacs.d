;;; init-js.el -- for javascript programming  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup typescript-ts-mode
  (:hooks typescript-ts-mode-hook eglot-ensure
					tsx-ts-mode-hook eglot-ensure))


(provide 'init-js)
;;; init-js.el ends here
