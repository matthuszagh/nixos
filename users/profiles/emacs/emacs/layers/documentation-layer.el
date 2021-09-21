;;; documentation-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def documentation
  :presetup
  (:layer straight
   (straight-use-package 'info-colors))

  :setup
  (use-package man
    :config
    (setq Man-notify-method 'pushy)
    (add-to-list 'same-window-buffer-names "*Man.*")
    (general-def 'normal Man-mode-map
      "RET" 'man-follow))

  (use-package info-colors
    :config
    (add-hook 'Info-selection-hook 'info-colors-fontify-node))

  (use-package info)

  :postsetup
  (:layer modal
   (general-define-key
    :states 'normal
    :keymaps 'Info-mode-map
    "H" 'Info-history
    "RET" 'Info-follow-nearest-node
    "g d" 'Info-follow-nearest-node
    "g p" 'Info-backward-node)

   (localleader :keymaps 'Info-mode-map
     "h" 'Info-backward-node
     "l" 'Info-forward-node)))


;;; documentation-layer.el ends here
