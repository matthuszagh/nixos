;;; documentation-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def documentation
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
    "H" 'Info-history)))

;;; documentation-layer.el ends here
