;;; parentheses-layer.el -*-lexical-binding: t; -*-

;;; Code:

(layer-def rainbow-delimiters
  :presetup
  (:layer straight
          (straight-use-package 'rainbow-delimiters))

  :setup
  (use-package rainbow-delimiters
    :functions rainbow-delimiters-mode
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package paren
    :config
    (show-paren-mode t)))

;;; parentheses-layer.el ends here
