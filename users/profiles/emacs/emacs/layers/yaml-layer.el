;;; yaml-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def yaml
  :presetup
  (:layer straight
   (straight-use-package 'yaml-mode))

  :setup
  (use-package yaml-mode))

;;; yaml-layer.el ends here
