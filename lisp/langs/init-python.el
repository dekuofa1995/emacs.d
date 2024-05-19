;;; init-python.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup eglot-python-
	(:with-feature eglot
		(:when-loaded
			;; npm install -g @delance/runtime
			(add-to-list 'eglot-server-programs
									 `((python-mode python-ts-mode) . ("delance-langserver" "--stdio")))
			;; (add-to-list 'eglot-server-programs
			;; 						 `((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
			))
	;; (add-hook 'python-mode-hook
	;; 					(lambda ()
	;; 						(unless (bound-and-true-p elpy-mode)
	;; 							(eglot-ensure))))
	;; (add-hook 'python-ts-mode-hook
	;; 					(lambda ()
	;; 						(unless (bound-and-true-p elpy-mode)
	;; 							(eglot-ensure))))
	)

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
    (deku/update-capf-backends '(python-mode python-ts-mo)
															 :company '(elpy-company-bac))))

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
		(setq
     org-babel-default-header-args:python '((:session . "py")
																						(:results . "output")))))

(defun +file-exist-in-proj (filename &optional proj-root)
	(let ((root (if proj-root proj-root (projectile-project-root))))
		(locate-file filename (list root))))

(setup python
	(:option*
	 python-shell-completion-native-disabled-interpreters '( "python" "pypy"))
	(:hooks
	 inferior-python-mode-hook corfu-mode))

;; python venv
(defun deku/pyvenv-workon ()
	(pyvenv-workon "."))
(setup pyvenv
	(:once '(:hooks python-mode-hook python-ts-mode-hook)
		(pyvenv-mode t))
	(:hooks (list python-mode-hook python-ts-mode-hook) deku/pyvenv-workon)
	(:when-loaded
		(defun pyvenv-workon-home+ ()
			"Return the current workon home.

This is the value of $WORKON_HOME or ~/.virtualenvs."
			(or (getenv "WORKON_HOME")
					(expand-file-name ".venv" (projectile-project-root))))

		(advice-add #'pyvenv-workon-home :override #'pyvenv-workon-home+)))

(setup poetry
	(:comment
	 (:once (list :hooks 'python-mode-hook 'python-ts-mode-hook)
		 (require 'poetry)
		 (poetry-tracking-mode))))

(setup conda
	(:comment
   (:hooks
		ein:ipynb-mode-hook conda-env-autoactivate-mode)
   (:autoload conda-env-list conda-env-activate conda-env-deactivate)
   (:option*
		conda-anaconda-home "/usr/local/Caskroom/miniconda/base/")))
;;
(setup ob-jupyter
	(:comment
	 (:once (list :files 'org)
		 (setq
			org-babel-default-header-args:jupyter-python '((:session . "py")
																										 (:kernal  . "python3"))))))

(defun deku/ein-separedit (ws cell)
	"Open separedit in ein's cell"
	(interactive (list
								(ein:worksheet--get-ws-or-error)
								(ein:worksheet-get-current-cell)))
	(let* ((beg (ein:cell-input-pos-min cell))
				 (end (ein:cell-input-pos-max cell))
				 (block (separedit-mark-region beg end)))
		(separedit-dwim block)))

(defun deku/ein-separedit (ws cell)
	"Open separedit in ein's cell"
	(interactive (list
								(ein:worksheet--get-ws-or-error)
								(ein:worksheet-get-current-cell)))
	(let* ((beg (ein:cell-input-pos-min cell))
				 (end (ein:cell-input-pos-max cell))
				 (block (separedit-mark-region beg end)))
		(separedit-dwim block)))

(setup ein
	;; to get kernel list, after ein:login see *ein:kernelspecs*
	(:option* ein:jupyter-default-kernel "ir")
  (:when-loaded
    (add-hook 'ein:notebook-mode-hook
							(lambda ()
								(define-key ein:notebook-mode-map (kbd "C-c C-'") #'deku/ein-separedit)
								(define-key ein:notebook-mode-map (kbd "C-M-n") #'ein:worksheet-goto-next-input-km)
								(define-key ein:notebook-mode-map (kbd "C-M-p") #'ein:worksheet-goto-prev-input-km))))
  (:autoload ein:run))

(provide 'init-python)
;;; init-python.el ends here
