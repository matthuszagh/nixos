;;; default-mode-line-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def default-mode-line
  :depends (base no-littering)
  :presetup
  (:layer straight
   (straight-use-package 'smart-mode-line))

  :setup
  (use-package smart-mode-line
    :after sourcerer-theme
    :config
    (setq size-indication-mode t)
    (setq column-number-mode t)
    (setq line-number-mode t)
    (setq sml/theme 'respectful)
    (setq sml/name-width 40)
    (sml/setup)))

;;; default-mode-line-layer.el ends here
