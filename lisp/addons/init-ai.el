;;; init-ai.el -- AI tools initialize file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup aidermacs
	(:autoload aidermacs-transient-menu)
	(:url "https://github.com/MatthewZMD/aidermacs")
	(:doc "Aidermacs: AI Pair Programming in Emacs")
	(:option* aidermacs-use-architect-mode t
						aidermacs-architect-model "deepseek/deepseek-reasoner"
						aidermacs-editor-model "deepseek/deepseek-chat")
	;; (:when-loaded
	;; 	(setenv "DEEPSEEK_API_KEY" deku/deepseek-api-key))
	(:global-bind "C-c a" aidermacs-transient-menu))


(provide 'init-ai)
;;; init-ai.el ends here
