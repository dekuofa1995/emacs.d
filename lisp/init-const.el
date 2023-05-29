;;; init-const.el -- Define constants. -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defconst my/config-backup-dir ".my-backup"
  "The configuration's backup directory path.")
(defconst my/config-dir ".my-config"
  "The configuration's diectory path.")
(defvar my/org-source-dir (file-name-directory (locate-library "org"))
  "Emacs Built-In ORG-MODE dir.")
(defvar my/config-force-reload nil
  "Force reload config switch.")
(defvar my/config-name-list '(laf editor note-taking programming addons private)
  "Congifuration file name list.
- laf: look and font
- editor: Emacs basic editor
- note-taking: note-taking relative packages, such as org, org-roam, denote
- programming: LSP, programming language's config
- addons: other kinds of package
- private: bind and other private configurations."
  )

(defvar my-show-icon t)

(provide 'init-const)
;;; init-const.el ends here
