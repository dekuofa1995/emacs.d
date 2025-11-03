;;; init-completion.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defvar deku/capf-backend-alist
  '((text-mode cape-dict)
    (prog-mode cape-keyword)
		(org-mode cape-keyword cape-tex))
  "An alist matching modes to company backends.

     The backends for any mode is built from this.")

(defvar deku/global-capf-backends '(cape-dabbrev cape-file)
  "An alist for global capf backends.")

(cl-defun deku/update-capf-backends (modes &key cape company)
  (if (and (null cape) (null company))
			;; as both backends are nil, clear backends for modes
      (let ((modes (enlist modes)))
				(dolist (m modes)
					(setq deku/capf-backend-alist
								(delq (assq m deku/capf-backend-alist)
											deku/capf-backend-alist))))
    (let* ((company (enlist company))
					 (cape (enlist cape))
					 (modes (enlist modes))
					 (comp-backend (if (null company)
														 (list)
													 (mapcar #'cape-company-to-capf company)))
					 (capfs (append cape comp-backend)))
			(dolist (m modes)
				(setf (alist-get m deku/capf-backend-alist)
							capfs)))))

(defun deku/capf-backends ()
  "Get completion at point function backends."
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend  (cdr (assq mode deku/capf-backend-alist)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in  deku/capf-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
																		(symbol-value mode))) ; minor modes
                        append (append deku/global-capf-backends backends ))
               (nreverse backends))))))

(defun deku/-setup-completions (capfs)
  "Merge c into completion-at-point-functions."
  (let ((capfs (enlist capfs)))
    (dolist (c capfs)
      (remove-hook 'completion-at-point-functions t t)
      (add-hook 'completion-at-point-functions c 1 'local))))

(defun deku/update-capf ()
  "Merge nasy capy."
  (require 'cape)
  (let ((backends (deku/capf-backends)))
    (deku/-setup-completions backends)))


(defun nasy/orderless-dispatch-flex-first (_pattern index _total)
  "orderless-flex for corfu."
  (and (eq index 0) 'orderless-flex))

(defun nasy/setup-corfu ()
  "Setup corfu."
  (setq-local orderless-matching-styles '(orderless-flex)
              orderless-style-dispatchers nil)
  (add-hook 'orderless-style-dispatchers #'nasy/orderless-dispatch-flex-first nil
            'local))

(setup company
  (:autoload company--multi-backend-adapter)
	(:global "C-M-/" company-files
					 "M-/" company-complete)
	(:with-map company-mode-map
		(:bind "C-c y" company-yasnippet))
	(:with-map company-active-map
		(:bind [tab] company-complete-selection
					 "TAB" company-complete-selection))
	(:also-load company-posframe yasnippet)
	(:hooks (list prog-mode-hook
								eglot-managed-mode-hook)
					(lambda ()
						(setq-local company-backends
												(append company-backends '((company-yasnippet))))))
	(global-company-mode))


(setup company-box
	(:comment
	 (:after company)
	 (:hooks company-mode-hook company-box-mode)))

(setup company-posframe
	(:when-loaded
		(company-posframe-mode)))

(setup corfu
	(:comment
	 (:load+ corfu)
   (:hooks (list prog-mode-hook
								 org-mode-hook
								 vterm-mode-hook
								 eval-expression-minibuffer-setup-hook)
					 nasy/setup-corfu)
	 (:after org
		 (defun deku/org-capf-setup ()
			 (setq-local completion-at-point-functions
									 (list (cape-capf-super
													#'cape-dabbrev
													#'cape-tex
													#'cape-dict))))
		 (add-hook 'org-mode-hook #'deku/org-capf-setup))
	 (:after eglot
		 ;; code from https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
		 (defun my/eglot-capf ()
			 (setq-local completion-at-point-functions
									 (list (cape-capf-super
													#'eglot-completion-at-point
													#'cape-file))))
		 (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))
   (:with-map corfu-map
     (:bind "C-g" corfu-quit
						"C-e" corfu-complete-common-or-next)
     (:unbind "<return>"))
   (:option* corfu-cycle t
						 corfu-auto t
						 corfu-auto-delay 0.4
						 corfu-auto-prefix 3
						 corfu-preview-current nil
						 text-mode-ispell-word-completion nil
						 tab-always-indent 'complete)
	 (global-corfu-mode)))

(setup corfu-popupinfo
  (:option*
	 corfu-popupinfo-delay '(0.8 . 0.5))
  (:hooks corfu-mode-hook corfu-popupinfo-mode))

(setup cape
	(:comment
   (:once (list :before 'corfu-mode)
     (add-hook 'completion-at-point-functions #'cape-file))
   (:hooks corfu-mode-hook deku/update-capf)
   (:global "C-M-/" cape-dabbrev
						"C-M-i" completion-at-point
						"M-/" completion-at-point)))

(setup orderless
	(:load-after vertico)
  (:option* completion-styles '(prescient orderless basic)))

(provide 'init-completion)
;;; init-completion.el ends here
