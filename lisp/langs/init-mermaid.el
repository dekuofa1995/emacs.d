;;; init-mermaid.el --  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup mermaid-mode
	(:url "https://github.com/abrochard/mermaid-mode")
	(:match-file ".mmd$"))

(setup org-mermaid
	(:option* ob-mermaid-cli-path "mmdc"))

(provide 'init-mermaid)
;;; init-mermaid.el ends here
