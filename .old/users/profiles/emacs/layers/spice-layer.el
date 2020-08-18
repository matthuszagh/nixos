;;; spice-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def spice
  :presetup
  (:layer straight
   (straight-use-package 'spice-mode))

  :setup
  (use-package spice-mode))

;;; spice-layer.el ends here
