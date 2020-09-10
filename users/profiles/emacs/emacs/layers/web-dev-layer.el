;;; web-dev-layer.el --- Web Development Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def web-dev
  :presetup
  (:layer straight
   (straight-use-package 'skewer-mode))

  :setup
  (use-package skewer-mode))

;;; web-dev-layer.el ends here
