;;; org-noter-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-noter
  :presetup
  (:layer straight
   (straight-use-package 'org-noter))

  :setup
  (use-package org-noter
    :config
    (setq org-noter-notes-search-path "~/doc/notes/wiki/refs")))

;;; org-noter-layer.el ends here
