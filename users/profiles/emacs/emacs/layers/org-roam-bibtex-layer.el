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
             :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}
#+CREATED: %(mh/time-stamp)
#+MODIFIED: %(mh/time-stamp)

* outline
:PROPERTIES:
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:END:
%(mh/pdf-outline-to-org-headline \"%(orb-process-file-field \"${citekey}\")\" 1 nil)

* references
<<bibliography link>>
bibliography:library.bib"
             :unnarrowed t)))))

;;; org-roam-bibtex-layer.el ends here
