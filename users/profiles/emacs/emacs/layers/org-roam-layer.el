;;; org-roam-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-roam
  :presetup
  (:layer straight
   (setq org-roam-v2-ack t)
   (straight-use-package 'org-roam)
   (straight-use-package '(org-roam-bibtex :branch "org-roam-v2")))

  :setup
  (use-package org-roam
    :config
    (org-roam-mode)
    (setq org-roam-directory "~/doc/notes/wiki")
    (setq org-roam-capture-templates
          `(("d" "default" plain "%?"
             :if-new
             ;; slug is a suitable converted filename (e.g. spaces
             ;; converted to underscores)
             (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        ,(concat ":PROPERTIES:\n"
                                 ":ID: %(org-id-new)\n"
                                 ":ROAM_ALIASES:\n"
                                 ":END:\n"
                                 "#+TITLE: ${title}\n"
                                 "#+filetags: \n"
                                 "#+CREATED: %(mh/time-stamp)\n"
                                 "#+MODIFIED: %(mh/time-stamp)\n\n"
                                 "* resources\n"
                                 "| link | description | type |\n"
                                 "| <l>  | <l>         | <c>  |\n"
                                 "| <40> | <40>        |      |\n"
                                 "|------+-------------+------|\n"
                                 "|      |             |      |\n\n"
                                 "** bibliography\n"
                                 "<<bibliography link>>\nbibliography:library.bib"))
             :unnarrowed t)
            ("r" "ref" plain
             ""
             :if-new
             (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        ,(concat "#+TITLE: ${title}\n"
                                 "#+filetags:\n"
                                 "#+CREATED: %(mh/time-stamp)\n"
                                 "#+MODIFIED: %(mh/time-stamp)\n\n"
                                 "* outline\n"
                                 ":PROPERTIES:\n"
                                 ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n"
                                 ":END:\n"
                                 "%(mh/pdf-outline-to-org-headline \"%(orb-process-file-field \"${citekey}\")\" 1)\n"
                                 "* references\n"
                                 "| link | description | type |\n"
                                 "| <l>  | <l>         | <c>  |\n"
                                 "| <40> | <40>        |      |\n"
                                 "|------+-------------+------|\n"
                                 "|      |             |      |\n\n"
                                 "** bibliography\n"
                                 "<<bibliography link>>\n"
                                 "bibliography:library.bib"))
             :unnarrowed t)))
    (defun mh//org-update-last-modified ()
      (save-excursion
        (goto-char 0)
        ;; MODIFIED must be present at buffer position 1000 or less,
        ;; to prevent accidentally changing a non-header property
        ;; value. Additionally, this avoids the computational cost of
        ;; traversing the full buffer.
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
    (org-roam-bibtex-mode))

  :postsetup
  (:layer modal
   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "i"
    :prefix-command 'mh/command-info-prefix
    :prefix-map 'mh/prefix-info-map
    "f" 'org-roam-node-find
    "i" 'org-roam-node-insert)

   (localleader :keymaps 'org-mode-map
     "r" 'org-roam)))

;;; org-roam-layer.el ends here
