;;; indenting-layer.el --- Indenting Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def indenting
  :presetup
  (:layer straight
   (straight-use-package 'aggressive-indent))

  :setup
  (use-package aggressive-indent
    :config
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

  :postsetup
  (:layer nix
   (add-to-list 'aggressive-indent-excluded-modes 'nix-mode))

  (:layer c
   (add-to-list 'aggressive-indent-excluded-modes 'c-mode)
   (add-to-list 'aggressive-indent-excluded-modes 'c++-mode)
   (add-to-list 'aggressive-indent-excluded-modes 'octave-mode)
   (add-to-list 'aggressive-indent-excluded-modes 'verilog-mode)))

;;; indenting-layer.el ends here
