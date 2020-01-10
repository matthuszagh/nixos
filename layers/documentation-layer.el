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
      "RET" 'man-follow)))

;;; documentation-layer.el ends here
