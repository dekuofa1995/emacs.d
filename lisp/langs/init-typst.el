;;; init-typst.el -- typst config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setup tinymist
	(:hooks typst-ts-mode-hook eglot-ensure)
  (:after* typst-ts-mode
		(require 'eglot)
		(add-to-list 'eglot-server-programs
								 '((typst-mode typst-ts-mode) . ("tinymist"))))
	(add-to-list 'auto-mode-alist '("\\.typ$" . typst-ts-mode)))

(setup typst-ts-mode
	(:option*
	 ;; typst-ts-mode-watch-options "--open"
	 typst-ts-mode-enable-raw-blocks-highlight t
	 typst-ts-mode-highlight-raw-blocks-at-startup t))

(setup tip
	(:once '(:hooks typst-ts-mode-hook)
		(require 'tip))
	(:hooks typst-ts-mode-hook tip-mode
					tip-mode-hook tip-ensure)
	(:option*
	 tip-server-port 8111
	 tip-server-basedir (file-truename "~/git/tools/tip-server-py/")))

;; tangle in typst from nasy https://emacs-china.org/t/tangle-typst-typst/27226
(defvar typst--initialized-files nil
  "List of files that have been initialized for the current tangle operation.")

(defun typst--initialize-file (path)
  "Initialize a temporary file for the PATH and return the temp file's path."
  (let ((temp-path (make-temp-file "typst" nil ".tmp")))
    (push (cons temp-path path) typst--initialized-files)
    temp-path))

(defun typst--tangle (node)
  "Recursively process each raw_blck NODE in the treesit parsed tree."
  (if (equal (treesit-node-type node) "raw_blck")
      (let* ((beg (treesit-node-start node))
             (path-line (treesit-node-text (treesit-node-on (- beg 2) (- beg 1))))
             (original-path (when (> (length path-line) 2) (string-trim (substring path-line 2))))
             (temp-path (when original-path
                          (or (car (rassoc original-path typst--initialized-files))
                              (typst--initialize-file original-path))))
             (content-node (car (treesit-filter-child node (lambda (n) (equal (treesit-node-type n) "blob")))))
             (content (when content-node (string-trim (treesit-node-text content-node)))))
        (when temp-path
          (append-to-file content nil temp-path)
          (append-to-file "\n" nil temp-path)
          (message "Tangled to temp file: %s." temp-path)))
    (dolist (child (treesit-node-children node))
      (typst--tangle child))))


(defun typst-tangle ()
  "Tangle the current typst file."
  (interactive)
  (setq typst--initialized-files nil)
  (let ((root (treesit-buffer-root-node)))
    (typst--tangle root)
    (dolist (pair typst--initialized-files)
      (let ((temp-path (car pair))
            (original-path (cdr pair)))
        (rename-file temp-path original-path t)
        (message "Moved %s to %s" temp-path original-path)))
    (setq typst--initialized-files nil)))
;; tangle end
(provide 'init-typst)
;;; init-typst.el ends here
