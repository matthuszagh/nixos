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
    (setq org-noter-notes-search-path "~/doc/notes/wiki/refs"))

  :postsetup
  (:layer modal
   (localleader :keymaps 'org-mode-map
     "n" 'org-noter)))

;;; org-noter-layer.el ends here
