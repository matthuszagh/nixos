;;; scad-layer.el --- SCAD Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def scad
  :presetup
  (:layer straight
   (straight-use-package 'scad-mode))

  :setup
  (use-package scad-mode))

;;; scad-layer.el ends here
