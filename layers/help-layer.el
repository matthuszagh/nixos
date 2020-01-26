;;; help-layer.el --- Help Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def help
  :presetup
  (:layer straight
   (straight-use-package 'helpful))

  :setup
  (use-package helpful
    :bind (("C-h f" . helpful-function)
           ("C-h v" . helpful-variable)
           ("C-h k" . helpful-key))
    :config
    (global-set-key (kbd "C-c C-d") #'helpful-at-point))

  :postsetup
  (:layer keybinding-management
   (general-def mh/prefix-help-map
     "h" 'helpful-at-point
     "f" 'helpful-function
     "v" 'helpful-variable
     "k" 'helpful-key
     "M" 'helpful-macro))

  (:layer (keybinding-management helm)
   (general-def mh/prefix-help-map
     "i" 'helm-info
     "m" 'helm-man-woman))

  (:layer windows
   (add-to-list 'display-buffer-alist
                '("\\*helpful.*" . (mh//display-popup-buffer-respect-monitors . ())))))

;;; help-layer.el ends here
