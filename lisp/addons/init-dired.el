;;; init-dired.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup dired
  (:hooks dired-mode-hook dired-omit-mode)
  (:with-map dired-mode-map
    (:bind
     "e" dired-previous-line
     "v" meow-visit
     "C-c C-w" wdired-change-to-wdired-mode
     "C-c C-n" dired-create-empty-file))
  (:option*
   ;; Guess a default taregt directory
   dired-dwim-target t
   ;; keep one dired buffer when change directory in dired
   dired-kill-when-opening-new-dired-buffer t
   ;; Always delete and copy recursively
   dired-recursive-copies 'always
   dired-recursive-deletes 'always

   ;; show the size of file by k/M/G & dirs first
   dired--listing-switches "-alh --group-directories-first")
  (:when-loaded
    (when sys/macp
      (if (executable-find "gls")
          (progn
            ;; Use GNU ls as `gls' from `coreutils' if available
            (setq insert-directory-program "gls")
            (setq ls-lisp-use-insert-directory-program t))
	(progn
          ;; Suppress the warning: `ls does not support --dired'.
          (setq dired-use-ls-dired nil)
          (setq dired-listing-switches "-alh --group-directories-first"))))))

(defun setup-dired-keymap ()
  "Setup My DIRED keymap."
  (define-key dired-mode-map (kbd "M-g h") (lambda () (interactive) (dired "~")))
  (define-key dired-mode-map (kbd "M-g n") (lambda () (interactive) (dired "~/Notes/Zk")))
  (define-key dired-mode-map (kbd "M-g p") (lambda () (interactive) (dired "~/Notes/Zk/projects")))
  (define-key dired-mode-map (kbd "M-g g") (lambda () (interactive) (dired "~/git"))))

(with-eval-after-load 'dired
  (setup-dired-keymap))

(setup dired-git-info
   (:with-map dired-mode-map
     (:bind  ")" dired-git-info-mode)))

(setup diredfl
  (:hooks dired-mode-hooks diredfl-mode))

(setup dired-aux
  (:hooks dired-mode-hook (lambda () (require 'dired-aux))))

(setup dired-x
  (:after dired
    (require 'dired-x)
    (let ((cmd (cond (sys/mac-x-p "open")
                     (sys/linux-x-p "xdg-open")
                     (sys/win32p "start")
                     (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))
    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*\\|~$"))))

(provide 'init-dired)
;;; init-dired.el ends here
