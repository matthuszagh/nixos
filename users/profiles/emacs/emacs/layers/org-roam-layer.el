;;; org-roam-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-roam
  :presetup
  (:layer straight
   (straight-use-package 'org-roam))

  :setup
  (use-package org-roam
    :config
    (org-roam-mode)
    (setq org-roam-directory "~/doc/notes/wiki")))

;;; org-roam-layer.el ends here
