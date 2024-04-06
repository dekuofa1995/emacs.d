;;; init-plantuml.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defconst deku/plantuml-jar-path
  (let* ((brew_prefix (if sys/x86p
		      "/usr/local"
		    "/opt/homebrew"))
     (jar-name "plantuml.jar")
     (jar-path (format "%s/opt/plantuml/libexec/%s" brew_prefix jar-name)))
(unless (file-exists-p jar-path)
  (message "Load plantuml from BREW failed, Please check it in BREW"))
jar-path))

(setup plantuml-mode
  (:option*
   plantuml-jar-path deku/plantuml-jar-path
   plantuml-default-exec-mode 'jar)
  (:after org-src
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))))

(setup ob-plantuml
	(:after 'ob
		(:option
		 org-plantuml-jar-path deku/plantuml-jar-path)))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
