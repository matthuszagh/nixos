;;; refactor-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def refactor
  :depends (programming)

  :presetup
  (:layer straight
   (straight-use-package 'emr)
   (straight-use-package 'wgrep))

  :setup
  ;; TODO do I need emr?
  (use-package emr)
  (use-package wgrep))

;;; refactor-layer.el ends here
