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
    (setq org-roam-directory "~/doc/notes/wiki")
    (setq org-roam-capture-templates
          `(("d" "default" plain #'org-roam-capture--get-point
             "%?"
             :file-name "${title}"
             :head ,(concat "#+title: ${title}\n"
                           "#+roam_alias: ")
             :unnarrowed t)))))

;;; org-roam-layer.el ends here
