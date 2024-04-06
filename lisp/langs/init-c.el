;;; init-c.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(with-eval-after-load 'eglot
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure))

(defun compile-and-run()
  (interactive)
  (let* ((src (file-name-nondirectory (buffer-file-name)))
         (exe (file-name-sans-extension src)))
    (compile (concat "clang " src " -o " exe) t)))

(with-eval-after-load 'c-ts-mode
  (define-key c-ts-mode-map (kbd "C-c C-r") #'compile-and-run))

(provide 'init-c)
;;; init-c.el ends here
