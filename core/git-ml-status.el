;;; git-ml-status.el --- Parse and render Git status  -*- lexical-binding: t; -*-

;;; Commentary:

;; Domain objects and pure helpers for `git-ml'.

;;; Code:

(require 'cl-lib)

(defgroup git-ml nil
  "Repository status in the mode line."
  :group 'mode-line)

(defgroup git-ml-faces nil
  "Faces used by `git-ml'."
  :group 'git-ml
  :group 'faces)

(defface git-ml '((t ())) "Default face." :group 'git-ml-faces)
(defface git-ml-action '((t (:inherit error :weight bold)))
  "Face used for an action in progress." :group 'git-ml-faces)
(defface git-ml-head '((t (:inherit success :weight bold)))
  "Face used for the current branch." :group 'git-ml-faces)
(defface git-ml-oid '((t (:inherit success :slant italic)))
  "Face used for a detached commit." :group 'git-ml-faces)
(defface git-ml-upstream '((t (:inherit success :slant italic)))
  "Face used for the upstream branch." :group 'git-ml-faces)
(defface git-ml-commits-ahead '((t (:inherit success)))
  "Face used for commits ahead." :group 'git-ml-faces)
(defface git-ml-commits-behind '((t (:inherit warning)))
  "Face used for commits behind." :group 'git-ml-faces)
(defface git-ml-files-staged '((t (:inherit success)))
  "Face used for staged files." :group 'git-ml-faces)
(defface git-ml-files-unstaged '((t (:inherit warning)))
  "Face used for unstaged files." :group 'git-ml-faces)
(defface git-ml-files-unmerged '((t (:inherit error :weight bold)))
  "Face used for unmerged files." :group 'git-ml-faces)
(defface git-ml-files-untracked '((t (:inherit warning)))
  "Face used for untracked files." :group 'git-ml-faces)

(cl-defstruct (git-ml-result (:constructor git-ml-result-create)
                             (:copier nil))
  action head oid upstream n-commits-ahead n-commits-behind
  (n-files-staged 0) (n-files-unstaged 0)
  (n-files-unmerged 0) (n-files-untracked 0))

(defun git-ml--git-action (git-dir)
  "Return the operation in progress in GIT-DIR, if any."
  (or
   (cl-loop for name in '("rebase-apply" "rebase" "../.dotest")
            for dir = (expand-file-name name git-dir)
            when (file-directory-p dir)
            return (cond
                    ((file-regular-p (expand-file-name "rebasing" dir)) "rebase")
                    ((file-regular-p (expand-file-name "applying" dir)) "am")
                    (t "am/rebase")))
   (cl-loop for name in '("rebase-merge/interactive"
                          ".dotest-merge/interactive")
            when (file-regular-p (expand-file-name name git-dir))
            return "rebase-i")
   (cl-loop for name in '("rebase-merge" ".dotest-merge")
            when (file-directory-p (expand-file-name name git-dir))
            return "rebase-m")
   (and (file-regular-p (expand-file-name "MERGE_HEAD" git-dir)) "merge")
   (and (file-regular-p (expand-file-name "BISECT_LOG" git-dir)) "bisect")
   (and (file-regular-p (expand-file-name "CHERRY_PICK_HEAD" git-dir))
        (if (file-directory-p (expand-file-name "sequencer" git-dir))
            "cherry-seq"
          "cherry"))
   (and (file-directory-p (expand-file-name "sequencer" git-dir))
        "cherry/revert")))

(defun git-ml--parse-header (record result)
  "Parse porcelain v2 header RECORD into RESULT."
  (when (string-match "\\`# \\([^ ]+\\) \\(.*\\)\\'" record)
    (let ((key (match-string 1 record))
          (value (match-string 2 record)))
      (cond
       ((string= key "branch.oid") (setf (git-ml-result-oid result) value))
       ((string= key "branch.head") (setf (git-ml-result-head result) value))
       ((string= key "branch.upstream")
        (setf (git-ml-result-upstream result) value))
       ((and (string= key "branch.ab")
             (string-match "\\`+\\([0-9]+\\) -\\([0-9]+\\)\\'" value))
        (setf (git-ml-result-n-commits-ahead result)
              (string-to-number (match-string 1 value))
              (git-ml-result-n-commits-behind result)
              (string-to-number (match-string 2 value))))))))

(defun git-ml--parse-status (output)
  "Parse NUL-delimited porcelain v2 OUTPUT into a `git-ml-result'."
  (let ((result (git-ml-result-create)))
    (dolist (record (split-string output "\0" t))
      (cond
       ((string-prefix-p "# " record) (git-ml--parse-header record result))
       ((and (> (length record) 3)
             (memq (aref record 0) '(?1 ?2))
             (= (aref record 1) ?\s))
        (unless (= (aref record 2) ?.)
          (cl-incf (git-ml-result-n-files-staged result)))
        (unless (= (aref record 3) ?.)
          (cl-incf (git-ml-result-n-files-unstaged result))))
       ((string-prefix-p "u " record)
        (cl-incf (git-ml-result-n-files-unmerged result)))
       ((string-prefix-p "? " record)
        (cl-incf (git-ml-result-n-files-untracked result)))))
    result))

(defun git-ml--branch-label (result)
  "Return the branch or detached commit label for RESULT."
  (let ((head (git-ml-result-head result))
        (oid (git-ml-result-oid result)))
    (if (equal head "(detached)")
        (if (equal oid "(initial)")
            "(initial)"
          (substring (or oid "unknown") 0 (min 7 (length (or oid "unknown")))))
      (or head "unknown"))))

(defun git-ml-render (result)
  "Render RESULT as a mode-line string."
  (when result
    (concat
     " "
     (when-let* ((action (git-ml-result-action result)))
       (propertize (format "[%s] " action) 'face 'git-ml-action))
     (propertize (git-ml--branch-label result) 'face 'git-ml-head)
     (mapconcat
      (lambda (item)
        (pcase-let ((`(,symbol ,number ,face) item))
          (when (and number (> number 0))
            (propertize (format " %s%d" symbol number) 'face face))))
      `(("✚" ,(git-ml-result-n-files-unstaged result) git-ml-files-unstaged)
        ("•" ,(git-ml-result-n-files-staged result) git-ml-files-staged)
        ("✖" ,(git-ml-result-n-files-unmerged result) git-ml-files-unmerged)
        ("?" ,(git-ml-result-n-files-untracked result) git-ml-files-untracked)
        ("↑" ,(git-ml-result-n-commits-ahead result) git-ml-commits-ahead)
        ("↓" ,(git-ml-result-n-commits-behind result) git-ml-commits-behind))
      ""))))

(provide 'git-ml-status)
;;; git-ml-status.el ends here
