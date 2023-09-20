;;; init-c.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(with-eval-after-load 'eglot
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure))

(setup ob-C
  (:autoload
   org-babel-execute:C
   org-babel-expand-body:C
   org-babel-execute:cpp
   org-babel-expand-body:cpp
   org-babel-execute:C++
   org-babel-expand-body:C++))

(defun compile-and-run()
  (interactive)
  (let* ((src (file-name-nondirectory (buffer-file-name)))
         (exe (file-name-sans-extension src)))
    (compile (concat "clang " src " -o " exe) t)))

(with-eval-after-load 'c-ts-mode
  (define-key c-ts-mode-map (kbd "C-c C-r") #'compile-and-run))

(provide 'init-c)
;;; init-c.el ends here
