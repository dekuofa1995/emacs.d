;;; git-ml-tests.el --- Tests for git-ml  -*- lexical-binding: t; -*-

(require 'ert)
(require 'git-ml)

(ert-deftest git-ml-parse-porcelain-v2-z ()
  (let* ((output (mapconcat
                  #'identity
                  '("# branch.oid 1234567890abcdef"
                    "# branch.head main"
                    "# branch.upstream origin/main"
                    "# branch.ab +2 -3"
                    "1 M. N... 100644 100644 100644 a b staged.el"
                    "1 .M N... 100644 100644 100644 a b modified.el"
                    "2 MM N... 100644 100644 100644 a b R100 renamed.el"
                    "old-name.el"
                    "u UU N... 100644 100644 100644 100644 a b c conflict.el"
                    "? untracked.el")
                  "\0"))
         (result (git-ml--parse-status output)))
    (should (equal (git-ml-result-head result) "main"))
    (should (equal (git-ml-result-upstream result) "origin/main"))
    (should (= (git-ml-result-n-commits-ahead result) 2))
    (should (= (git-ml-result-n-commits-behind result) 3))
    (should (= (git-ml-result-n-files-staged result) 2))
    (should (= (git-ml-result-n-files-unstaged result) 2))
    (should (= (git-ml-result-n-files-unmerged result) 1))
    (should (= (git-ml-result-n-files-untracked result) 1))))

(ert-deftest git-ml-status-command-disables-optional-locks ()
  (should (equal (seq-take git-ml--status-command 2)
                 '("git" "--no-optional-locks")))
  (should (member "--porcelain=v2" git-ml--status-command))
  (should (member "--branch" git-ml--status-command))
  (should (member "-z" git-ml--status-command)))

(ert-deftest git-ml-cache-is-shared-by-repository ()
  (let ((git-ml--repositories (make-hash-table :test #'equal))
        (root (file-name-as-directory (file-truename default-directory))))
    (should (eq (git-ml--repo-for-directory root)
                (git-ml--repo-for-directory (expand-file-name "core" root))))
    (should (= (hash-table-count git-ml--repositories) 1))))

(ert-deftest git-ml-coalesces-refresh-while-process-is-live ()
  (let* ((git-ml--repositories (make-hash-table :test #'equal))
         (repo (git-ml--repo-create :root default-directory
                                    :git-dir "/tmp/git-ml-test/.git/"))
         (process (make-pipe-process :name "git-ml-test-live" :noquery t)))
    (unwind-protect
        (progn
          (setf (git-ml--repo-process repo) process)
          (git-ml--request-refresh repo)
          (should (git-ml--repo-pending repo))
          (should-not (git-ml--repo-refresh-timer repo)))
      (when (process-live-p process) (delete-process process)))))

(ert-deftest git-ml-refreshes-real-repository-asynchronously ()
  (let ((git-ml--repositories (make-hash-table :test #'equal))
        (git-ml-refresh-delay 0)
        (git-ml-process-timeout 5)
        (root (file-name-as-directory (file-truename default-directory))))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory root)
          (git-ml-activate)
          (let* ((repo (git-ml--current-repo))
                 (deadline (+ (float-time) 5)))
            (while (and (not (git-ml--repo-result repo))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (should (git-ml--repo-result repo))
            (should (stringp (git-ml--repo-rendered repo)))
            (should (equal git-ml (git-ml--repo-rendered repo)))
            (should-not (process-live-p (git-ml--repo-process repo)))))
      (git-ml-clear-cache))))

(provide 'git-ml-tests)
;;; git-ml-tests.el ends here
