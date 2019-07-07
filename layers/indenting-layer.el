;;; indenting-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def indenting
  :presetup
  (:layer straight
          (straight-use-package 'aggressive-indent))

  :setup
  (use-package aggressive-indent
    :config
    (global-aggressive-indent-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'nix-mode)))

;;; indenting-layer.el ends here
