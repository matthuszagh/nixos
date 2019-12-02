;;; org-ql-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-ql
  :setup
  (use-package org-ql)
  (use-package helm-org-ql)

  :postsetup
  (:layer (helm org modal)
   (setq mh-org-wiki-file "/home/matt/doc/notes/wiki.org")
   (defun mh/helm-global-search ()
     (interactive)
     (helm :sources `(,(helm-org-ql-source (list mh-org-wiki-file))
                      ,(helm-def-source--info-files)
                      helm-source-man-pages
                      helm-source-recoll-library)))

   (general-def mh/prefix-search-map
     "g" 'mh/helm-global-search)))

;;; org-ql-layer.el ends here
