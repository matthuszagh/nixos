;;; hydra-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def hydra
  :presetup
  (:layer straight
   (straight-use-package 'hydra))

  :setup
  (use-package hydra))

;;; hydra-layer.el ends here
