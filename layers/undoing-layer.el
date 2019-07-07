;;; undoing-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def undoing
  :setup
  (use-package undo-tree
    :config
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t))

  :postsetup
  (:layer keybinding-management
          (general-def mh/prefix-undo-map
            "u" 'undo-tree-undo
            "r" 'undo-tree-redo
            "U" 'undo-tree-visualize)))

;;; undoing-layer.el ends here
