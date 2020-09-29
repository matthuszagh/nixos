;;; djvu-layer.el --- DJVU Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def djvu
  :presetup
  (:layer straight
   (straight-use-package 'djvu))

  :setup
  ())

;;; djvu-layer.el ends here
