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
  (:global
   "C-c n n"      denote
   "C-c n N"      denote-type
   "C-c n d"      denote-date
   "C-c n z"      denote-signature
   "C-c n s"      denote-subdirectory
   "C-c n r"      denote-rename-file
   "C-c n i"      denote-link
   "C-c n I"      denote-link-add-links
   "C-c n b"      denote-link-backlinks
   "C-c n f f"    denote-link-find-file
   "C-c n f b"    denote-link-find-backlink)
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
