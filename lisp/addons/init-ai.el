;;; init-ai.el -- AI tools initialize file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup aider
	(:url "https://github.com/tninja/aider.el?tab=readme-ov-file")
	(:doc "aider.el : aider (AI Pair Programming) Inside Emacs")
	(:option*
	 aider-args '("--model" "deepseek"))
	(:when-loaded
		(setenv "DEEPSEEK_API_KEY" deku/deepseek-api-key))
	(:global
	 "C-c a" aider-transient-menu))


(provide 'init-ai)
;;; init-ai.el ends here
