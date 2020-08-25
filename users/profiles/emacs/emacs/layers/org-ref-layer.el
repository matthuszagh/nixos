;;; org-ref-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-ref
  :presetup
  (:layer straight
   (straight-use-package 'org-ref))

  :setup
  (use-package org-ref))

;;; org-ref-layer.el ends here
