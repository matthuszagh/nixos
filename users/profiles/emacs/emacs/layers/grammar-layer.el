;;; grammar-layer.el --- Grammar Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def grammar
  :presetup
  (:layer straight
   (straight-use-package 'bnf-mode)
   (straight-use-package 'bison-mode))

  :setup
  (use-package bnf-mode)
  (use-package bison-mode))

(provide 'grammar-layer)
;;; grammar-layer.el ends here
