(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centaur-tabs-excluded-prefixes
   '("*epc" "*helm" "*Helm" " *which" "*Compile-Log*" "*lsp" "*LSP" "*company" "*Ediff" "*ediff" "*tramp" " *Mini" "*straight" " *temp"))
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "iMac.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" default))
 '(java-ts-mode-indent-offset 2)
 '(projectile-ignored-projects '("~/"))
 '(python-indent-offset 4)
 '(python-shell-completion-native-disabled-interpreters '("pypy"))
 '(python-shell-completion-native-enable t)
 '(python-shell-interpreter "python")
 '(safe-local-variable-values
   '((conda-project-env-path . "pydata-book")
     (ffip-prune-patterns "*/lib" "*/devdocs" "*/dist" "*/racket-mode" "*/dirvish" "*/newsticker" "*/url" "*/transient" "*/tree-sitter" "*/autosave-list" "*/.cache" "*/eln-cache" "*/elpa" "*/straight" "*/.git" "*/.svn" "*/.cvs" "*/.tox" "*/.bzr" "*/.hg" "*/.DS_Store" "*/.sass-cache" "*/elpy" "*/dcache" "*/.npm" "*/.tmp" "*/.idea" "*/node_modules" "*/bower_components" "*/.gradle" "*/.cask"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#51afef"))))
 '(meow-beacon-indicator ((t (:background "#FF8800" :foreground "white"))))
 '(meow-insert-indicator ((t (:background "#acf2bd" :foreground "black"))))
 '(meow-keypad-indicator ((t (:background "#ffc86f" :foreground "white"))))
 '(meow-motion-indicator ((t (:background "#51afef" :foreground "white"))))
 '(meow-normal-indicator ((t (:background "#51afef" :foreground "white"))))
 '(meow-search-indicator ((t (:background "#c678dd" :foreground "white"))))
 '(mode-line ((t (:height 0.9))))
 '(mode-line-active ((t (:height 0.9))))
 '(mode-line-inactive ((t (:height 0.9))))
 '(rime-code-face ((t (:inherit font-lock-string-face :height 200))))
 '(typst-mode-markup-heading-1-face ((t (:weight bold :height 240))))
 '(typst-mode-markup-heading-2-face ((t (:inherit typst-mode-markup-heading-1-face :height 210))))
 '(typst-mode-markup-heading-3-face ((t (:inherit typst-mode-markup-heading-1-face :height 190))))
 '(typst-mode-markup-heading-4-face ((t (:inherit typst-mode-markup-heading-1-face :height 180))))
 '(typst-mode-markup-heading-5-face ((t (:inherit typst-mode-markup-heading-1-face :height 160))))
 '(variable-pitch ((t :font-family "Iosevka"))))
