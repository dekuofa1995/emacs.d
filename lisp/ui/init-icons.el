;;; init-icons.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(setup nerd-icons
  (:require nerd-icons)
  (:when-loaded
    ;; fix orig. nerd dashboard oct icon missing
    (let ((icons nerd-icons-mode-icon-alist))
      (setq nerd-icons-mode-icon-alist
            (cons '(benchmark-init/tree-mode nerd-icons-codicon "nf-cod-dashboard"
                                             :face nerd-icons-blue)
                  (delq (assq 'benchmark-init/tree-mode icons) icons))))))

(setup nerd-icons-dired
  (:hooks dired-mode-hook nerd-icons-dired-mode))

(setup nerd-icons-ibuffer
  (:hooks ibuffer-mode-hook nerd-icons-ibuffer-mode))

(defun setup-vscode-kind-icons ()
  "Setup vscode style's kind-icons."
  (interactive)
  (setq kind-icon-mapping
  '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
    (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
    (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
    (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
    (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
    (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
    (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
    (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
    (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
    (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
    (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
    (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
    ;; add statement for python
    (statement      "st"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
    (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
    (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
    (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
    (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
    (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
    (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
    (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
    (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
    (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
    ;; add instance for python
    (namespace      "ns"  :icon "file-code-outline"  :face font-lock-preprocessor-face)
    (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
    (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
    (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
    (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
    (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
    (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
    (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
    (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
    (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
    (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
    (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
    (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
    (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
    (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
    ;; add instance for python
    (instance       "in"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
    (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode"))))

(setup kind-icon
  (:require kind-icon)
  (:option*
   kind-icon-use-icons t
   kind-icon-blend-background nil
   ;; fix kind icon to large bug
   kind-icon-default-face 'corfu-default ; to compute blended backgrounds correctly
   kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.2))
  (:after corfu
    (setup-vscode-kind-icons)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(provide 'init-icons)
;;; init-icons.el ends here
