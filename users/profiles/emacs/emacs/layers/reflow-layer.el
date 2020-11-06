;;; reflow-layer.el --- Reflow Layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Text reflow and wrapping functionality.

;;; Code:

(layer-def reflow
  :presetup
  (:layer straight
   (straight-use-package 'visual-fill-column))

  :setup
  (use-package visual-fill-column
    :config
    (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
    (setq visual-fill-column-width 70)))

;;; reflow-layer.el ends here
