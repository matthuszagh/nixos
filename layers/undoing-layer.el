;;; undoing-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def undoing
  :setup
  (use-package undo-tree
    :config
    (global-undo-tree-mode)
    (setq-default undo-tree-visualizer-diff t)
    ;; Save undo information
    (setq undo-tree-auto-save-history t))

  :postsetup
  (:layer keybinding-management
   (general-def mh/prefix-undo-map
     "u" 'undo-tree-undo
     "r" 'undo-tree-redo
     "U" 'undo-tree-visualize)))

;;; undoing-layer.el ends here
