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


(setup claude-code-ide
	(:global-bind "C-c C-a" claude-code-ide-menu))

(setup codex-ide
	(:global-bind "C-c C-;" codex-ide-menu))

(setup gptel
	(:option*
	 gptel-model 'deepseek-v4-flash
	 gptel-backend (gptel-make-deepseek "DeepSeek"
									 :stream t
									 :key deku/deepseek-api-key)))

(setup gptel-magit
	(:hooks magit-mode-hook gptel-magit-install))
(provide 'init-ai)
;;; init-ai.el ends here
