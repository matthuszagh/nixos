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
             :head ,(concat "#+TITLE: ${title}\n"
                            "#+ROAM_ALIAS: \n"
                            "#+CREATED: %(format-time-string \"[%Y-%m-%d %a %H:%M]\")\n"
                            "#+MODIFIED: %(format-time-string \"[%Y-%m-%d %a %H:%M]\")\n")
             :unnarrowed t)))
    (defun mh//org-update-last-modified ()
      (save-excursion
        (goto-char 0)
        (search-forward "MODIFIED: ")
        (delete-region (point) (line-end-position))
        (let ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now))))
    (add-hook 'org-mode-hook (lambda ()
                               (add-hook 'before-save-hook 'mh//org-update-last-modified nil t)))))

;;; org-roam-layer.el ends here
