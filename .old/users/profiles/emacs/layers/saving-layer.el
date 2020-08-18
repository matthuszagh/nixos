;;; saving-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def saving
  :setup
  (use-package super-save
    :config
    (super-save-mode 1)
    (setq super-save-auto-save-when-idle t)))

;;; saving-layer.el ends here
