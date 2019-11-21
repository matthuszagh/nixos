;;; rainbow-delimiters-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

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

;;; rainbow-delimiters-layer.el ends here
