;;; init-note-misc.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup markdown-mode
  (:with-map markdown-mode-map
    "C-x C-v" #'markdown-toggle-markup-hiding))
(setup bibtex
  (:load-after org)
  (:when-loaded (:option bibtex-file-path "~/Dropbox/roam/bib/"
                         bibtex-files '("bibtex.bib")
                         bibtex-notes-path "~/Dropbox/roam/cards/"
                         bibtex-align-at-equal-sign t
                         bibtex-autokey-titleword-separator "-"
                         bibtex-autokey-year-title-separator "-"
                         bibtex-autokey-name-year-separator "-"
                         bibtex-dialect 'biblatex)))

(setup ebib
	(:load-after bibtex)
	(:option* ebib-default-directory bibtex-file-path
						ebib-bib-search-dirs `(,bibtex-file-path)
						ebib-file-search-dirs `(,(concat bibtex-file-path "files"))
						ebib-notes-directory bibtex-notes-path
						ebib-reading-list-file (file-truename "~/Dropbox/roam/reading.org")
            ebib-bibtex-dialect bibtex-dialect
            ebib-file-associations '(("pdf" . "open"))
            ebib-index-default-sort '("timestamp" . descend)
            ebib-reading-list-project-marker "PROJECT"
            ebib-notes-template ":PROPERTIES:\n:ID: %i\n:ROAM_REFS: @%k\n:END:\n#+title: %t\n#+description: %d\n#+date: %s\n%%?\n"
            ebib-notes-template-specifiers '((?k . ebib-create-key)
                                             (?i . ebib-create-id)
                                             (?t . ebib-create-org-title)
                                             (?d . ebib-create-org-description)
                                             (?l . ebib-create-org-link)
                                             (?s . ebib-create-org-time-stamp))
            ebib-reading-list-template "* %M %T\n:PROPERTIES:\n%K\n:END:\n%F\n%S\n"
            ebib-reading-list-template-specifiers '((?M . ebib-reading-list-project-marker)
                                                    (?T . ebib-create-org-title)
                                                    (?K . ebib-reading-list-create-org-identifier)
                                                    (?F . ebib-create-org-file-link)
                                                    (?S . ebib-create-org-stamp-inactive))
            ebib-preload-bib-files bibtex-files
            ebib-use-timestamp t))

(setup citar
	(:load-after org)
	(:option org-cite-global-bibliography '("~/Dropbox/roam/bib/bibtex.bib")
           citar-notes-paths (list "~/Dropbox/roam/cards")
           citar-library-paths (list "~/Dropbox/roam/bib/files")
           org-cite-insert-processor 'citar
           org-cite-follow-processor 'citar
           org-cite-activate-processor 'citar
           citar-bibliography org-cite-global-bibliography)
  (:when-loaded
    (defvar citar-indicator-files
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-file_o"
                :face 'nerd-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-link"
                :face 'nerd-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes
      (citar-indicator-create
       :symbol (nerd-icons-codicon
                "nf-cod-note"
                :face 'nerd-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "    "
       :tag "has:notes"))
    (defvar citar-indicator-cited
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-circle_o"
                :face 'nerd-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))))

(provide 'init-note-misc)
;;; init-note-misc.el ends here
