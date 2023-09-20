;;; init-meow.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(let ((normal-keybindings '(("?" "meow-keypad-describe-key") (1 "select-window-or-expand-1") (2 "select-window-or-expand-2") (3 "select-window-or-expand-3") (4 "select-window-or-expand-4") (5 "select-window-or-expand-5") (6 "select-window-or-expand-6") (7 "select-window-or-expand-7") (8 "select-window-or-expand-8") (9 "select-window-or-expand-9") (0 "select-window-or-expand-0") ("-" "negative-argument") (";" "meow-reverse") ("," "meow-inner-of-thing") ("." "meow-bounds-of-thing") ("`/" "meow-visit") ("[" "meow-beginning-of-thing") ("]" "meow-end-of-thing") ("a" "meow-append") ("A" "meow-open-below") ("b" "meow-back-word") ("B" "meow-back-symbol") ("c" "meow-change") ("C" "meow-change-save") ("d" "meow-delete") ("D" "meow-backward-delete") ("e" "meow-prev") ("E" "meow-prev-expand") ("g" "meow-cancel-selection") ("G" "meow-grab") ("h" "meow-left") ("H" "meow-left-expand") ("i" "meow-right") ("I" "meow-right-expand") ("j" "meow-join") ("k" "meow-kill") ("l" "meow-line") ("m" "meow-mark-word") ("M" "meow-mark-symbol") ("n" "meow-next") ("N" "meow-next-expand") ("o" "mewo-block") ("O" "meow-to-block") ("p" "meow-yank") ("q" "meow-quit") ("r" "meow-replace") ("R" "meow-replace-save") ("s" "meow-insert") ("S" "meow-open-below") ("t" "meow-till") ("u" "vundo") ("U" "meow-undo-in-selection") ("v" "meow-search") ("w" "meow-next-word") ("W" "meow-next-symbol") ("x" "meow-delete") ("X" "meow-backward-delete") ("y" "meow-save") ("z" "meow-pop-selection") ("'" "repeat") ("&" "meow-query-replace-regexp") ("%" "meow-query-replace") ("<escape>" "ignore")))
      (motion-keybindings '(("<escape>" "ignore") (1 "winum-select-window-1") (2 "winum-select-window-2") (3 "winum-select-window-3") (4 "winum-select-window-4") (5 "winum-select-window-5") (6 "winum-select-window-6") (7 "winum-select-window-7") (8 "winum-select-window-8") (9 "winum-select-window-9") (0 "winum-select-window-0")))
      (leader-keybindings '(("D" "global-command-log-mode" "debug emacs command") ("e" "meow-eval-last-exp" "") ("'" "meow-wrap-string" "") ("@ u" "smerge-keep-upper" "") ("@ l" "smerge-keep-lower" "") ("@ a" "smerge-keep-all" "") ("@ m" "smerge-keep-mine" "") ("@ o" "smerge-keep-other" "") (";" "meow-comment" "") ("(" "meow-wrap-round" "") ("[" "meow-wrap-square" "") ("{" "meow-wrap-curly" "") ("}" "meow-forward-barf" "") (")" "meow-forward-slurp" "") ("R" "meow-transpose-sexp" "") ("i" "imenu-list" "") ("F" "find-file-in-project-at-point" "") ("l" "consult-ripgrep" "") ("L" "meow-goto-line" "") ("p" "find-file-in-project" "") ("q" "delete-window" "") ("T" "vterm" "") ("j" "meow-join-sexp" "") ("w" "other-window" "") ("W" "ace-window" "") ("b" "consult-buffer" "") ("o" "delete-other-windows" "") ("s" "meow-splice-sexp" "") ("S" "meow-split-sexp" "") ("k" "kill-current-buffer" "") ("K" "kill-matching-buffers" "") ("v" "magit" "") ("-" "split-window-below" "") ("`=" "split-window-right" "") ("," "meow-pop-marker" "") ("." "meow-find-ref" "") ("u" "meow-universal-argument" "") (1 "meow-digit-argument" "") (2 "meow-digit-argument" "") (3 "meow-digit-argument" "") (4 "meow-digit-argument" "") (5 "meow-digit-argument" "") (6 "meow-digit-argument" "") (7 "meow-digit-argument" "") (8 "meow-digit-argument" "") (9 "meow-digit-argument" "") (0 "meow-digit-argument" ""))))
(defvar normal-keybindings normal-keybindings)
(defvar motion-keybindings motion-keybindings)
(defvar leader-keybindings leader-keybindings)

(declare-function meow-leader-define-key           "ext:meow-helpers.el")
(declare-function meow-normal-define-key           "ext:meow-helpers.el")
(declare-function meow-motion-overwrite-define-key "ext:meow-helpers.el")

(defun meow-setup ()
  (let ((parse-def (lambda (x)
                     (cons
                      (format "%s" (if (and (stringp (car x)) (string-prefix-p "`" (car x)))
                                       (string-trim (substring (car x) 1))
                                     (car x)))
                      (if (string-prefix-p "dispatch:" (cadr x))
                          (string-trim (substring (cadr x) 9))
                        (intern (cadr x)))))))
    (apply #'meow-leader-define-key (mapcar parse-def leader-keybindings))
    (apply #'meow-normal-define-key (mapcar parse-def normal-keybindings))
    (apply #'meow-motion-overwrite-define-key (mapcar parse-def motion-keybindings))))
)

(setup meow
  (:once (list :hooks 'after-init-hook)
    (require 'meow))
  (:option*
   meow-esc-delay 0.001
   meow-char-thing-table
	 '((?\( .	round)
		 (?\) .	round)
		 (?\" . string)
		 (?\[ .	square)
		 (?\] .	square)
		 (?<  .	angle)
		 (?>  .	angle)
		 (?{  .	curly)
		 (?}  .	curly)
		 (?s  .	symbol)
		 (?f  .	defun)
		 (?w  .	window)
		 (?l  .	line)
		 (?b  .	buffer)
		 (?p  .	paragraph)))

  (:when-loaded
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
    ;; TODO use dolist
    (add-to-list 'meow-mode-state-list '(dashboard-mode . motion))
    (add-to-list 'meow-mode-state-list '(info-mode . normal))
    (add-to-list 'meow-mode-state-list '(calculator-mode . insert))
    (add-to-list 'meow-mode-state-list
								 '(cargo-process-mode . motion))
    (add-to-list 'meow-mode-state-list
								 '(benchmark-init/tabulated-mode . normal))
    (add-to-list 'meow-mode-state-list
								 '(benchmark-init/tree-mode . normal))
    (meow-setup)
    ;; (meow-setup-line-number)

    (unless (bound-and-true-p meow-global-mode)
      (meow-global-mode 1))))

(provide 'init-meow)
;;; init-meow.el ends here
