;;; init-python.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	 `((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-hook 'python-ts-mode-hook
      (lambda ()
	(unless (bound-and-true-p elpy-mode)
	  (eglot-ensure)))))

(defun elpy-setup ()
      "Setup ELPY."
      (interactive)
      (elpy-enable)
      (elpy-mode))
(setup elpy
  (:autoload elpy-enable)
  (:option*
   elpy-modules '(elpy-module-sane-defaults elpy-module-company elpy-module-eldoc))
  (:when-loaded
    (deku/update-capf-backends '(python-mode python-ts-mode)
      :company '(elpy-company-backend)))
  (:hooks
   ein:notebook-mode-hook elpy-setup))

(defun deku/-elpy-module-company (command &rest _args)
  "Module to support company-mode completions."
  (pcase command
    (`global-init
     (require 'company)
     (require 'company-capf)
     (elpy-modules-remove-modeline-lighter 'company-mode)
     (define-key company-active-map (kbd "C-d")
								 'company-show-doc-buffer)
     (add-hook 'inferior-python-mode-hook
               (lambda ()
                 ;; Workaround for company bug
                 ;; (https://github.com/company-mode/company-mode/issues/759)
                 (setq-local company-transformers
                             (remove 'company-sort-by-occurrence
                                     company-transformers))
                 ;; Be sure to trigger completion for one character variable
                 ;; (i.e. `a.`)
                 (setq-local company-minimum-prefix-length 2))))

    (`buffer-init
     ;; We want immediate completions from company.
     (set (make-local-variable 'company-idle-delay)
          0.1)
     ;; And annotations should be right-aligned.
     (set (make-local-variable 'company-tooltip-align-annotations)
          t)
     ;; Also, dabbrev in comments and strings is nice.
     (set (make-local-variable 'company-dabbrev-code-everywhere)
          t)
     ;; Add our own backend and remove a bunch of backends that
     ;; interfere in Python mode.
     ;; (set (make-local-variable 'company-backends)
     ;;      (cons 'elpy-company-backend
     ;;            (delq 'company-semantic
     ;;                  (delq 'company-ropemacs
     ;;                        (delq 'company-capf
     ;;                              (mapcar #'identity company-backends))))))
     ;; (company-mode 1)
     (when (> (buffer-size) elpy-rpc-ignored-buffer-size)
       (message
        (concat "Buffer %s larger than elpy-rpc-ignored-buffer-size (%d)."
                " Elpy will turn off completion.")
        (buffer-name) elpy-rpc-ignored-buffer-size)))
    (`buffer-stop
     ;; (company-mode -1)
     (kill-local-variable 'company-idle-delay)
     (kill-local-variable 'company-tooltip-align-annotations)
     (kill-local-variable 'company-backends))
    ))
(defun deku/setup-elpy ()
	(define-minor-mode elpy-mode
		"Minor mode in Python buffers for the Emacs Lisp Python Environment.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[pyvenv-workon], you can use
\\[elpy-rpc-restart] to make the elpy Python process use your
virtualenv.

\\{elpy-mode-map}"
		:lighter " Elpy"
		(unless (or (derived-mode-p 'python-mode 'python-ts-mode)
								(cl-find 'ein:notebook-mode local-minor-modes))
			(error "Elpy only works with `python-mode'"))
		(unless elpy-enabled-p
			(error "Please enable Elpy with `(elpy-enable)` before using it"))
		(when (boundp 'xref-backend-functions)
			(add-hook 'xref-backend-functions #'elpy--xref-backend nil t))
		;; Set this for `elpy-check' command
		(setq-local python-check-command elpy-syntax-check-command)
		(cond
		 (elpy-mode
			(elpy-modules-buffer-init))
		 ((not elpy-mode)
			(elpy-modules-buffer-stop))))

	(advice-add #'elpy-module-company :override #'deku/-elpy-module-company))

(setup deku/elpy
	(:once (list :files 'elpy)
		(deku/setup-elpy)))

(setup ob-python
  (:once (list :files 'org)
    (require 'ob-python))
  (:option*
    org-babel-default-header-args:python '((:async   . "yes")
                                           (:session . "py")
                                           (:results . "output")
                                           (:kernal  . "python")))
  (:autoload org-babel-execute:python
             org-babel-expand-body:python))

(setup conda
  (:hooks
   ein:ipynb-mode-hook conda-env-autoactivate-mode)
  (:autoload conda-env-list conda-env-activate conda-env-deactivate)
  (:option*
   conda-anaconda-home "/usr/local/Caskroom/miniconda/base/"))

(setup jupyter
  (:autoload
   org-babel-execute:jupyter
   org-babel-expand-body:jupyter))

(setup ein
  (:when-loaded
    (add-hook 'ein:notebook-mode-hook
	      (lambda ()
		(define-key ein:notebook-mode-map (kbd "C-M-n") #'ein:worksheet-goto-next-input-km)
		(define-key ein:notebook-mode-map (kbd "C-M-p") #'ein:worksheet-goto-prev-input-km))))
  (:autoload ein:run))

(provide 'init-python)
;;; init-python.el ends here
