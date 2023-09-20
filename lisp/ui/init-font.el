;;; init-font.el -- Init File. -*- lexical-binding: t -*-
;;; Commentary:

(defvar deku/font-size 20
  "Default font pt size.")
(defvar deku/modeline-font-size 14
  "Default font pt size.")
;; fonts
(defvar deku/eng-font "Iosevka"
  "English font family.")
(defvar deku/cn-font "LXGW WenKai"
  "Chinese font family.")
(defvar deku/modeline-font "Menlo"
  "Modeline font family.")
(defvar deku/emoji-font "Apple Color Emoji"
  "Emoji font family.")

;;;###autoload
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is installed."
  (find-font (font-spec :name font-name)))

;;;###autoload
(defun deku/font-height (font-size)
  "Convert FONT-SIZE to font height in Emacs."
  (* font-size 10))

;;;###autoload
(defmacro deku/font--ensure (font-name &rest body)
  "Ensure font with FONT-NAME is available before execute BODY."
  `(progn
    (if (font-installed-p ,font-name)
  (progn ,@body)
     (warn (format "Cann't find Font: %s!" ,font-name)))))

(defun deku/setup-fonts ()
  "Setup fonts by DEKU/ENG-FONT, DEKU/CN-FONT, DEKU/MODELINE-FONT, DEKU/EMOJI-FONT with corresponding size.

ENG-FONT and CN-FONT use DEKU/FONT-SIZE
MODELINE-FONT use DEKU/MODELINE-FONT-SIZE"
  (interactive)
  (deku/font--ensure deku/eng-font
                     (set-face-attribute 'default nil :family deku/eng-font
                                         :height (deku/font-height deku/font-size)))

  (deku/font--ensure deku/cn-font
                     (set-fontset-font t '(#x4e00 . #x9fff)
                                       (font-spec :family deku/cn-font
                                                  :height (deku/font-height deku/font-size))))
  (deku/font--ensure deku/emoji-font (set-fontset-font t 'emoji (font-spec :family deku/emoji-font) nil 'prepend))

  (deku/font--ensure deku/modeline-font
                     (progn
                       (set-face-attribute 'mode-line nil
                                           :family deku/modeline-font
                                           :height (deku/font-height deku/modeline-font-size))
                       (set-face-attribute 'mode-line-active nil
                                           :weight 'medium)
                       (set-face-attribute 'mode-line-inactive nil
                                           :weight 'light))))

(add-hook 'after-init-hook
          #'deku/setup-fonts) ;; for normal emacs
(add-hook 'server-after-make-frame-hook #'deku/setup-fonts) ;; for emacs --demand

(set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(provide 'init-font)
;;; init-font.el ends here
