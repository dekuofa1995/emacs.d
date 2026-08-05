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
	(:when-loaded
		(deku/load-dotenv (expand-file-name ".env" user-emacs-directory)))
	(:option*
	 gptel-model 'gpt-5.6-sol
	 gptel-request-extra '((reasoning_effort . high)
												 (service_tier . fast))
	 gptel-backend (gptel-make-openai "CLIProxy"
									 :host (getenv "CODEX_HOST")
									 :endpoint "/v1/chat/completions"
									 :stream t
									 :models '("gpt-5.6-sol" "gpt-5.6-luna")
									 :key (getenv "CODEX_API_KEY"))))

(setup gptel-magit
	(:hooks magit-mode-hook gptel-magit-install))
(provide 'init-ai)
;;; init-ai.el ends here
