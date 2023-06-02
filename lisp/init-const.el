;;; init-const.el -- Define constants. -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defconst my/config-backup-dir ".my-backup"
  "The configuration's backup directory path.")
(defconst my/config-dir ".my-config"
  "The configuration's diectory path.")
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
(defvar my/english-font "Iosevka"
  "English Font Family Name.")

(defvar setup-benchmark nil
  "Wheather start benchmark for Emacs.")
;; Constants from centaur
(defconst my/org-source-dir (file-name-directory (locate-library "org"))
  "Emacs Built-In ORG-MODE dir.")
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")
(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")


(defvar my-show-icon t)

(provide 'init-const)
;;; init-const.el ends here
