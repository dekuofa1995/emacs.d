;;; init-clojure.el -- Deku Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup clojure-mode
  (:hooks clojure-mode-hook eglot-ensure)
  (:option*
    clojure-toplevel-inside-comment-form t)
  (:when-loaded
    (define-clojure-indent
     (re-frame/reg-event-fx :defn)
     (re-frame/reg-event-db :defn)
     (re-frame/reg-sub :defn)
     (re-frame/reg-fx :defn)
     (t/async :defn)
     (thrown-with-msg? :defn))))

(setup cider
  (:option*
   cider-offer-to-open-cljs-app-in-browser nil
   cider-show-error-buffer -1)
  (:hooks cider-repl-mode-hook enable-paredit-mode
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
