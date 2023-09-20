;;; init-embark.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup embark
  (:after 'consult
    (require 'embark))
  (:global
   "C-."   embark-act
   "C-h B" embark-bindings ;; alternative for `describe-bindings'
   "C-M-'" embark-dwim)
  ;; :init
  ;; for integration with `which-key'
  ;; see https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  (:when-loaded
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))
  (:with-feature which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
  The which-key help message will show the type and value of the
  current target followed by an ellipsis if there are further
  targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator)))

(setup embark-consult
  (:once (list :packages 'embark)
    (require 'embark-consult))
  (:hooks
    embark-collect-mode-hook consult-preview-at-point-mode))

(provide 'init-embark)
;;; init-embark.el ends here
