;;; multiple-cursors-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def multiple-cursors
  :presetup
  (:layer straight
          (straight-use-package 'multiple-cursors))

  :setup
  (electric-pair-mode 1)

  (use-package multiple-cursors
    :config
    ;; TODO what does this do?
    (setq mc/always-repeat-command t))

  :postsetup
  (:layer keybinding-management
          (general-def
            "M-j" 'mc/mark-next-like-this
            "M-k" 'mc/mark-previous-like-this)))

;;; multiple-cursors-layer.el ends here
