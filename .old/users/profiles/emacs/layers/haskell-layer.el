;;; haskell-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def haskell
  :presetup
  (:layer straight
   (straight-use-package 'haskell-mode))

  :setup
  (use-package haskell-mode))

;;; haskell-layer.el ends here
