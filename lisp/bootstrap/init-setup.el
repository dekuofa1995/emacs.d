;;; init-setup.el -- Deku Init File. -*- lexical-binding: t -*-
;;; Commentary:

(cl-eval-when (compile)
  (require 'borg)
  (require 'info))

(require 'setup)
;; from once https://github.com/emacs-magus/once
(require 'once-setup)

(defvar empty-fn
  (lambda (&rest _) nil)
  "Empty function for base shortcuts, do nothing but comment.")

;; base shortcuts
(setup-define :doc
  empty-fn
  :documentation "The shortcut for `:documentation' for setup.")

(setup-define :comment
  empty-fn
  :documentation "Comment the code in body.")

(setup-define :tag
  empty-fn
  :documentation "The tag(s) for classify the package.")

(setup-define :url
  empty-fn
  :documentation "The url for the package home page.")

(setup-define :quit
  #'setup-quit
  :documentation "The shortcut for `setup-quit'.")

(setup-define :option*
  (setup-make-setter
   (lambda (name)
     `(funcall (or (get ',name 'custom-get)
                   #'symbol-value)
               ',name))
   (lambda (name val)
     `(progn
        (custom-load-symbol ',name)
        (funcall (or (get ',name 'custom-set) #'set-default)
                 ',name ,val))))

  :documentation "Like default `:option', but set variables after the feature is
loaded."
  :debug '(sexp form)
  :repeatable t
  :after-loaded t)

(setup-define :hooks
  (lambda (hooks func)
    (let ((hooks (if (listp hooks) hooks (list hooks)))
					bs)
      (dolist (hook hooks)
				(push `(add-hook ',hook #',func) bs))
      `(progn ,@bs)))
  :documentation "Add pairs of hooks."
  :repeatable t)

(setup-define :init
  (lambda (&rest body) (macroexp-progn body))
  :documentation "Init keywords like use-pacakge.")

(setup-define :load-after
  (lambda (&rest features)
    (let ((body `(require ',(setup-get 'feature))))
      (dolist (feature (nreverse features))
				(setq body `(with-eval-after-load ',feature ,body)))
      body))
  :documentation "Load the current feature after FEATURES.")

(setup-define :silence
  (lambda (&rest body)
    `(cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
       ,(macroexp-progn body)))
  :documentation "Evaluate BODY but keep the echo era clean."
  :debug '(setup))

;; (setup-define :load-first
;;   (lambda (&rest features)
;;     `(:first-key*
;;       (:load-after ,@features)))
;;   :documentation "Load the current feature with `:first-key*' after FEATURES.")

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "Eval BODY after FEATURE and the current feature."
  :after-loaded t
  :indent 1)

(setup-define :delay
  (lambda (time &rest body)
    `(run-with-idle-timer ,time nil
                          (lambda () ,@body)))
  :documentation "Delay loading BODY until a certain amount of idle time
has passed."
  :indent 1)

(setup-define :advice
  (lambda (symbol where func)
    `(advice-add ',symbol ,where ,func))
  :documentation "Add a piece of advice on SYMBOL.
See `advice-add' for more details."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

(setup-define :mode-hook
  (lambda (&rest body)
    `(add-hook ',(setup-get 'hook)
							 #'(lambda () (progn ,@body))))
  :documentation "Add BODY to the current mode hook.")

(setup-define :autoload
  (lambda (func)
    (let ((fn (if (memq (car-safe func) '(quote function))
									(cadr func)
								func)))
      `(unless (fboundp (quote ,fn))
				 (autoload (function ,fn)
					 ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload COMMAND if not already bound."
  :repeatable t
  :signature '(FUNC ...))

(setup-define :load+
  (lambda (&rest packages)
    `(elemacs-load-packages-incrementally '(,@packages)))
  :documentation "Load packages incrementally.")

(provide 'init-setup)
;;; init-setup.el ends here
