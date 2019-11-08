;;; asy-layer.el --- Summary -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def asy
  :setup
  (use-package asy-mode
    :mode (("\\.asy\\'" . asy-mode))))

;;; asy-layer.el ends here
