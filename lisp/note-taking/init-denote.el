;;; init-denote.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:


(setup denote
  (:option*
   denote-directory "~/Notes/Zk"
   denote-dired-mode t
   denote-file-type ".org"
   denote-templates
   `((review . "* Some heading\n\n* Another heading")
     (memo . ,(concat "* Some heading"
                      "\n\n"
                      "* Another heading"
                      "\n\n"))))
	(:after transient
		(transient-define-prefix transient-map-denote ()
			"DENOTE."
			[["NEW"
				("nn" "normal" denote)
				("nt" "type" denote-type)
				("nd" "date" denote-date)
				("nz" "signature" denote-signature)
				("ns" "subdir" denote-subdirectory)]
			 ["EDIT"
				("rn" "rename" denote-rename-file)
				"🢆 LINK"
				("l" "link" denote-link)
				("L" "links" denote-link-add-links)
				("bl" "back" denote-link-backlinks)]
			 ["FIND"
				("ff" "file" denote-link-find-file)
				("fb" "back" denote-link-find-backlink)]])
		(:global
		 "C-x d"      transient-map-denote))
  (:with-map dired-mode-map
		"C-c C-d C-i"  #'denote-link-dired-marked-notes
		"C-c C-d C-r"  #'denote-dired-rename-marked-files))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("d" "New note (with denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

(provide 'init-denote)
;;; init-denote.el ends here
