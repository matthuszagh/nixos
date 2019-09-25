;;; refactor-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def refactor
  :depends (programming)

  :setup
  ;; TODO do I need emr?
  (use-package emr)
  (use-package wgrep))

;;; refactor-layer.el ends here
