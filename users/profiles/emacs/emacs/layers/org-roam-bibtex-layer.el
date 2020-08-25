;;; org-roam-bibtex-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-roam-bibtex
  :depends (org-roam)

  :presetup
  (:layer straight
   (straight-use-package 'org-roam-bibtex))

  :setup
  (use-package org-roam-bibtex
    :hook
    (org-roam-mode . org-roam-bibtex-mode)
    :config
    (setq orb-templates
          '(("r" "ref" plain #'org-roam-capture--get-point
             ""
             :file-name "refs/${citekey}"
             :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n"
             :unnarrowed t)))))

;;; org-roam-bibtex-layer.el ends here
