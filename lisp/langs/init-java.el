;;; init-java.el -- Deku Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defconst deku/jdtls-lsp-cache-dir (file-name-concat user-emacs-directory ".cache" "jdtls-lsp-cache"))
(setup jdtls-eglot-setup
  (:hooks (java-mode-hook java-ts-mode-hook) eglot-ensure)
  (:with-feature 'projectile
    (:autoload 'projectile-project-root))
  (:with-feature 'eglot
    (:when-loaded
      (defun jdtls-command-contact (&optional interactive)
				(let* ((jdtls-cache-dir deku/jdtls-lsp-cache-dir)
               (project-dir (file-name-nondirectory
														 (directory-file-name (projectile-project-root))))
               (data-dir (expand-file-name (file-name-concat jdtls-cache-dir (md5 project-dir))))
               (jvm-args `("-Xmx8G"
													 "-XX:+UseZGC"
													 "-XX:+UseStringDeduplication"
													 ;; "-XX:FreqInlineSize=325"
													 ;; "-XX:MaxInlineLevel=9"
													 "-XX:+UseCompressedOops"))
               (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
               ;; tell jdtls the data directory and jvm args
               (contact (append '("jdtls") jvm-args `("-data" ,data-dir))))
					contact))
      (push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs))))

(provide 'init-java)
;;; init-java.el ends here
