;;; json-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def json
  :presetup
  (:layer straight
   (straight-use-package 'json-mode))

  :setup
  (use-package json-mode))

;;; json-layer.el ends here
