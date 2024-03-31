;;; init-const.el -- Define constants. -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;;; Variables
;;; Constants
(defconst deku/org-source-dir (file-name-directory (locate-library "org"))
  "Emacs Built-In ORG-MODE dir.")
(defconst deku/vertico-load-path
  (expand-file-name "lib/vertico/extensions" user-emacs-directory)
  "Vertico Extensions dir.")
(defconst deku/corfu-extensions-load-path
  (expand-file-name "lib/corfu/extensions" user-emacs-directory)
  "Corfu Extensions dir.")
;; Constants from centaur
(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")
(defconst sys/x86p
  (string-match "^x86_64" system-configuration)
  "Are we running on a x86 system?")
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

(defvar setup-benchmark nil
  "Wheather start benchmark for Emacs.")

(defvar deku/conf-path (expand-file-name "lisp" user-emacs-directory)
  "Path for Emacs config directories.")

(defvar deku/conf-dirs '("bootstrap" "ui" "editor" "langs" "note-taking" "addons")
  "An alist of directories for Emacs config.

The order of the alist is the load order when Emacs init.")

(defvar deku/theme 'kaolin-light
  "Current Theme.")
(defvar deku/show-icon t
  "Switch for show icon.")

(defconst deku/note-dir "~/Notes"
	"The directory that stores notes")

(defconst deku/roam-dir (file-truename "~/Dropbox/Roam")
	"Org Roam Note's directory.")

(defconst deku/note-para-file "PARA.org"
	"PARA filename")

(provide 'init-const)
;;; init-const.el ends here
