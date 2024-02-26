;;; init-clojure.el -- Deku Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defun user/clj-comment--commented? ()
	(equal "#_" (buffer-substring-no-properties (point) (+ 2 (point)))))
(defun user/clj-comment--closure ()
	(save-mark-and-excursion
		(unless (and (equal (char-after) ?\()
								 (equal pos (line-beginning-position)))
			(beginning-of-defun)
			(if (equal "#_" (buffer-substring-no-properties (point) (+ 2 (point))))
					(delete-char 2)
				(insert "#_")))))

(cl-defun user/clj-comment--expr (&optional (num 1))
	(save-mark-and-excursion
		(if (user/clj-comment--commented?)
				(while (user/clj-comment--commented?)
					(delete-char 2))
			(let* ((sym (symbol-at-point))
						 (sym-name (symbol-name sym))
						 (line (buffer-substring-no-properties (point) (line-end-position))))

				(unless (or (string-prefix-p sym-name line)
										(equal ?\( (char-after))
										(equal ?\[ (char-after))
										(equal ?\{ (char-after)))
					(backward-sexp))
				(dotimes (_ num)
					(insert "#_"))))))

(defun user/clj-comment ()
	"Insert/Remove Comment in clojure."
	(interactive)
	(let ((arg current-prefix-arg))
		(cond
		 ((not arg)
			(skip-chars-forward "\s-")
			(user/clj-comment--expr))
		 ((numberp arg)
			(skip-chars-forward "\s-")
			(user/clj-comment--expr arg))
		 ;; with C-u prefix
		 ((equal '(4) arg) (user/clj-comment--closure)))))



(setup clojure-mode
  (:hooks clojure-mode-hook eglot-ensure)
	(:with-map clojure-mode-map
		(:bind
		 "C-#" user/clj-comment))
  (:option*
   clojure-toplevel-inside-comment-form t))

(defun deku/cider-auto-scroll ()
	(setq  scroll-conservatively 101))

(setup cider
  (:option*
   cider-offer-to-open-cljs-app-in-browser nil
   cider-show-error-buffer -1
	 cider-repl-buffer-size-limit 5000)
  (:hooks cider-repl-mode-hook enable-paredit-mode
					cider-repl-mode-hook deku/cider-auto-scroll
					cider-repl-mode-hook corfu-mode)
  (:with-map cider-mode-map
    (:bind "C-c C-f" cider-format-buffer))
  (:with-map cider-repl-mode-map
    (:bind "S-<return>" newline)))

(setup queue)

(setup parseclj)

(setup parseedn)

(setup sesman)

(provide 'init-clojure)
;;; init-clojure.el ends here
