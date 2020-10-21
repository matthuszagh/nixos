;;; icon-font-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def icon-font
  :presetup
  (:layer straight
   (straight-use-package 'all-the-icons))

  :setup
  (use-package all-the-icons))

;;; icon-font-layer.el ends here
