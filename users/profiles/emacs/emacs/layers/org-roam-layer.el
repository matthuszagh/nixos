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
        (search-forward "MODIFIED: ")
        (delete-region (point) (line-end-position))
        (let ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now))))
    (add-hook 'org-mode-hook (lambda ()
                               (add-hook 'before-save-hook
                                         'mh//org-update-last-modified 0 t))))

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
