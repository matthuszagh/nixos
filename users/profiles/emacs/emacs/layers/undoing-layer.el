;;; undoing-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def undoing
  :presetup
  (:layer straight
   (straight-use-package 'undo-tree))

  :setup
  ;; Increase undo limits
  (setq undo-limit 16000000)
  (setq undo-strong-limit 24000000)

  (use-package undo-tree
    :hook
    (undo-tree-visualizer-mode . (lambda ()
                                   (auto-save-mode 1)))
    :config
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)
    ;; Save undo information persistently (i.e. across sessions)
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-visualizer-timestamps t))

  :postsetup
  (:layer keybinding-management
   (general-def mh/prefix-undo-map
     "u" 'undo-tree-undo
     "r" 'undo-tree-redo
     "U" 'undo-tree-visualize)))

;;; undoing-layer.el ends here
