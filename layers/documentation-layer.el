;;; documentation-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def documentation
  :setup
  (use-package man
    :config
    (setq Man-notify-method 'pushy)
    (add-to-list 'same-window-buffer-names "*Man.*")))

;;; documentation-layer.el ends here
