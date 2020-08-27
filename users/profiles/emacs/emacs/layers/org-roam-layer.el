;;; org-roam-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-roam
  :presetup
  (:layer straight
   (straight-use-package 'org-roam)
   (straight-use-package 'org-roam-bibtex))

  :setup
  (use-package org-roam
    :config
    (org-roam-mode)
    (setq org-roam-directory "~/doc/notes/wiki")
    (setq org-roam-capture-templates
          `(("d" "default" plain #'org-roam-capture--get-point
             "%?"
             ;; slug is a suitable converted filename (e.g. spaces
             ;; converted to underscores)
             :file-name "${slug}"
             :head ,(concat "#+TITLE: ${title}\n"
                            "#+ROAM_ALIAS: \n"
                            "#+ROAM_TAGS: \n"
                            "#+CREATED: %(mh/time-stamp)\n"
                            "#+MODIFIED: %(mh/time-stamp)\n")
             :unnarrowed t)))
    (defun mh//org-update-last-modified ()
      (save-excursion
        (goto-char 0)
        (if (search-forward "MODIFIED: " 1000 t)
            (progn
              (delete-region (point) (line-end-position))
              (let ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
                (insert now))))))
    (add-hook 'org-mode-hook (lambda ()
                               (add-hook 'before-save-hook
                                         'mh//org-update-last-modified 0 t))))

  (use-package org-roam-bibtex
    :config
    (add-hook 'org-roam-mode-hook 'org-roam-bibtex-mode)
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
             :unnarrowed t))))

  :postsetup
  (:layer modal
   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "i"
    :prefix-command 'mh/command-info-prefix
    :prefix-map 'mh/prefix-info-map
    "f" 'org-roam-find-file
    "i" 'org-roam-insert)

   (localleader :keymaps 'org-mode-map
     "r" 'org-roam)))

;;; org-roam-layer.el ends here
