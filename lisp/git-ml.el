;;; Asynchronous detailed git modeline.

;; git-ml is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This is a rough sketch. Things to do before publishing it as a package:
;;
;; * Consider doing the checks per git directory instead of per buffer, then
;;   propagating the results to all buffers which use this directory. Read
;;   vc-mode's implementation -- does it do things this way or just calculate
;;   the modeline for every buffer separately?
;;
;; * Distinguish changes in the current file from changes in other files. Use
;;   brackets, box, underline, etc. For example, display " master ✚1 [•1]" when
;;   the current file has a staged change and there's another file with an
;;   unstaged change.
;;
;; * Handle errors by stopping the remaining processes and passing nil to
;;   git-ml-render-function.
;;
;; * Ensure that we never start the processes multiple times, even if the user
;;   does many refreshes in quick succession.
;;
;; * Use a timeout for the processes.
;;
;; * Kill the processes when they're no longer needed because the user has
;;   killed the buffer(s).
;;
;; * Hook into magit to refresh the modeline after it executes commands.
;;
;; * Refresh the git state in an idle timer.
;;
;; * Document the result struct in git-ml-render-function's docstring.
;;
;; * Consider using one of the async/await libraries. They don't seem popular
;;   though, so not sure if it's a good idea.
;;
;; * Consider turning the file into a global minor mode.
;;
;; * Consider changing the git-ml prefix.

;; How to use:
;; 1. Add `git-ml' to your mode line, e.g. `(... vc-mode git-ml ...)'
;; 2. If you only use `vc-mode' for its modeline, you probably want to disable
;;    it for git: `(setq vc-handled-backends (delq 'Git vc-handled-backends))'.
;; 3. `(git-ml-activate)'.

;; Inspiration (ideas):
;; * https://github.com/romkatv/powerlevel10k
;; * https://github.com/yonchu/zsh-vcs-prompt

;; Inspiration (implementation):
;; * https://github.com/zsh-users/zsh/blob/2f2aa36/Functions/VCS_Info/Backends/VCS_INFO_get_data_git
;; * https://github.com/zsh-users/zsh/blob/2f2aa36/Misc/vcs_info-examples
;; * https://kitchingroup.cheme.cmu.edu/blog/2014/09/19/A-git-status-Emacs-modeline

(require 'dash)
(require 'cl-lib)

(defvar git-ml-debug nil
  "Whether to show debug messages.")

(defsubst git-ml--debug (format-string &rest args)
  (when git-ml-debug
    (apply #'message (concat "git-ml: " format-string) args)))

(defvar git-ml-check-git-action-p t
  "Should we check for the git action in progress?
This requires some synchronous file accesses that may pause Emacs
if the filesystem with the repo is very slow.")

(defun git-ml--get-git-action (git-dir)
  "Return the current git action in progress, or nil if there's none.
Example return values: \"rebase\", \"rebase-i\", \"cherry\".
GIT-DIR should be the path to the .git directory."
  ;; Logic from
  ;; <https://github.com/zsh-users/zsh/blob/2f2aa36/Functions/VCS_Info/Backends/VCS_INFO_get_data_git>.
  ;; I haven't found an async way to do this.
  (or
   (--some
    (let ((dir (expand-file-name it git-dir)))
      (when (file-directory-p dir)
        (or
         (and (file-regular-p (expand-file-name "rebasing" dir)) "rebase")
         (and (file-regular-p (expand-file-name "applying" dir)) "am")
         "am/rebase")))
    '("rebase-apply" "rebase" "../.dotest"))

   (--some
    (let ((file (expand-file-name it git-dir)))
      (when (file-regular-p file)
        "rebase-i"))
    '("rebase-merge/interactive" ".dotest-merge/interactive"))

   (--some
    (let ((dir (expand-file-name it git-dir)))
      (when (file-directory-p dir)
        "rebase-m"))
    '("rebase-merge" ".dotest-merge"))

   (and (file-regular-p (expand-file-name "MERGE_HEAD" git-dir)) "merge")

   (and (file-regular-p (expand-file-name "BISECT_LOG" git-dir)) "bisect")

   (and (file-regular-p (expand-file-name "CHERRY_PICK_HEAD" git-dir))
        (or (and (file-directory-p (expand-file-name "sequencer" git-dir))
                 "cherry-seq")
            "cherry"))

   (and (file-directory-p (expand-file-name "sequencer" git-dir))
        "cherry/revert")))

(defvar git-ml nil
  "Part of the modeline with git information.")
(make-variable-buffer-local 'git-ml)

;; Structs have to be defined before we try to `setf' their slots, otherwise
;; we'll get errors like `Symbol’s function definition is void: \(setf\
;; git-ml-result-action\)'.

(cl-defstruct (git-ml--state (:constructor git-ml--state-create)
                             (:copier nil))
  buffer
  process-dir
  remaining-processes)

(cl-defstruct (git-ml-result (:constructor git-ml-result-create)
                             (:copier nil))
  (action nil)
  (head nil)
  (oid nil)
  (upstream nil)
  (n-commits-ahead nil)
  (n-commits-behind nil)
  (n-files-staged 0)
  (n-files-unstaged 0)
  (n-files-unmerged 0)
  (n-files-untracked 0))

(defvar git-ml-render-function #'git-ml-render
  "Function to use to render the git-ml variable. It will be
called with the result struct, or nil if there was an error
getting git information. It should return a string or nil.")

(defun git-ml-render (result)
  (git-ml--debug "Rendering: %S" result)
  (when result
    (concat
     " "
     (when (git-ml-result-action result)
       (format "[%s] " (git-ml-result-action result)))
     (if (string= (git-ml-result-head result) "(detached)")
         (if (string= (git-ml-result-oid result) "(initial)")
             "(initial)"
           (substring (git-ml-result-oid result) 0 7))
       (git-ml-result-head result))
     (mapconcat
      (lambda (symbol-and-number)
        (let ((symbol (car symbol-and-number))
              (number (cdr symbol-and-number)))
          (when (not (memq number '(nil 0)))
            (concat " " symbol (number-to-string number)))))
      (list
       (cons "✚" (git-ml-result-n-files-unstaged result))
       (cons "•" (git-ml-result-n-files-staged result))
       (cons "✖" (git-ml-result-n-files-unmerged result))
       (cons "?" (git-ml-result-n-files-untracked result))
       (cons "↑" (git-ml-result-n-commits-ahead result))
       (cons "↓" (git-ml-result-n-commits-behind result)))
      ""))))

(defun git-ml--maybe-finish (state result)
  "If we've finished gathering information, set the modeline variable."
  (cl-assert (>= (git-ml--state-remaining-processes state) 0))
  (when (and (zerop (git-ml--state-remaining-processes state))
             (buffer-live-p (git-ml--state-buffer state)))
    (with-current-buffer (git-ml--state-buffer state)
      (setf git-ml (funcall git-ml-render-function result))
      (git-ml--debug "Rendered: %S" git-ml))))

(defun git-ml--parse-git-status (buffer result)
  (with-current-buffer buffer
    (goto-char (point-min))
    (save-match-data
      (while (not (eobp))
        (git-ml--debug "git status line: %s"
                       (buffer-substring-no-properties
                        (point) (line-end-position)))
        (cond
         ;; Optional headers, e.g. `# branch.ab +0 -0'.
         ((looking-at "# \\([^ ]+\\) \\(.+\\)$")
          (let ((key (match-string-no-properties 1))
                (value (match-string-no-properties 2)))
            (cond
             ((string= key "branch.oid")
              (setf (git-ml-result-oid result) value))
             ((string= key "branch.head")
              (setf (git-ml-result-head result) value))
             ((string= key "branch.upstream")
              (setf (git-ml-result-upstream result) value))
             ((string= key "branch.ab")
              (if (string-match "^\\+\\([0-9]+\\) -\\([0-9]+\\)$" value)
                  (progn
                    (setf (git-ml-result-n-commits-ahead result)
                          (string-to-number (match-string-no-properties 1 value)))
                    (setf (git-ml-result-n-commits-behind result)
                          (string-to-number (match-string-no-properties 2 value))))
                (error "Can't parse value for branch.ab: %s" value))))))
         ;; Changes, e.g. `1 .M N... 100644 (...) git-ml.el'.
         ((looking-at "\\(?:1\\|2\\) \\(.\\)\\(.\\) ")
          (when (not (string= (match-string-no-properties 1) "."))
            (cl-incf (git-ml-result-n-files-staged result)))
          (when (not (string= (match-string-no-properties 2) "."))
            (cl-incf (git-ml-result-n-files-unstaged result))))
         ;; Unmerged files, e.g. `u UU N... 100644 (...) git-ml.el'.
         ((looking-at "u ")
          (cl-incf (git-ml-result-n-files-unmerged result)))
         ;; Untracked files, e.g. `? git-ml.el'.
         ((looking-at "\\? ")
          (cl-incf (git-ml-result-n-files-untracked result))))
        (forward-line)))))

(defun git-ml--status-sentinel (process msg)
  (when (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process))
             (buffer-live-p (process-buffer process)))
    (let ((state (process-get process 'git-ml--state-struct))
          (result (process-get process 'git-ml--result-struct)))
      (git-ml--parse-git-status (process-buffer process) result)
      (kill-buffer (process-buffer process))
      (cl-decf (git-ml--state-remaining-processes state))
      (git-ml--maybe-finish state result))))

(defun git-ml--git-dir-sentinel (process msg)
  (when (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process))
             (buffer-live-p (process-buffer process)))
    (let ((state (process-get process 'git-ml--state-struct))
          (result (process-get process 'git-ml--result-struct)))
      (with-current-buffer (process-buffer process)
        (goto-char (point-max))
        (when (save-match-data (looking-back "\n"))
          (delete-char -1))
        (let* ((git-dir (buffer-substring-no-properties (point-min) (point-max)))
               (abs-git-dir (expand-file-name
                             git-dir (git-ml--state-process-dir state)))
               (action (git-ml--get-git-action abs-git-dir)))
          (setf (git-ml-result-action result) action))
        (kill-buffer))
      (cl-decf (git-ml--state-remaining-processes state))
      (git-ml--maybe-finish state result))))

(defun git-ml-refresh ()
  "Refresh git state of the current buffer."
  (interactive)
  (let* ((default-directory (if (buffer-file-name)
                                (file-name-directory (buffer-file-name))
                              default-directory))
         (state-struct
          (git-ml--state-create
           :buffer (current-buffer)
           :process-dir default-directory
           :remaining-processes (if git-ml-check-git-action-p 2 1)))
         (result-struct (git-ml-result-create))
         (status-process
          (make-process :name "git status for modeline"
                        :buffer " *git status for modeline*"
                        ;; Ignore dirty submodules because we wouldn't do anything with them.
                        :command '("git" "status" "--porcelain=2"
				   ;; "--branch" ;; hide branch
                                   "--ignore-submodules=dirty")
                        :connection-type 'pipe
                        :sentinel #'git-ml--status-sentinel
                        :noquery t))
         (git-dir-process
          (when git-ml-check-git-action-p
            (make-process :name "git rev-parse for modeline"
                          :buffer " *git rev-parse for modeline*"
                          :command '("git" "rev-parse" "--git-dir")
                          :connection-type 'pipe
                          :sentinel #'git-ml--git-dir-sentinel
                          :noquery t))))
    ;; Will this always execute before a sentinel runs? I think so, but not sure.
    (dolist (process (list status-process git-dir-process))
      (when process
        (process-put process 'git-ml--state-struct state-struct)
        (process-put process 'git-ml--result-struct result-struct)))))

(defun git-ml-activate ()
  (interactive)
  (add-hook 'find-file-hook #'git-ml-refresh)
  (add-hook 'after-save-hook #'git-ml-refresh))

(provide 'git-ml)
