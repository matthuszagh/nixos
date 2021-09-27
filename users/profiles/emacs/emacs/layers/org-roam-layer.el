;;; org-roam-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-roam
  :presetup
  (:layer straight
   (setq org-roam-v2-ack t)
   (straight-use-package 'org-roam)
   (straight-use-package 'org-roam-bibtex))

  :setup
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

  (use-package org-roam
    :hook ((org-mode . org-roam-db-autosync-mode)
           (org-mode . (lambda ()
                         (add-hook 'before-save-hook
                                   'mh//org-update-last-modified 0 t))))
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
    ;; TODO also look at `org-format-outline-path'. Probably not
    ;; exactly what I want, but it does something similar.
    (setq mh//org-roam-helm-tags-width 25)
    (cl-defmethod org-roam-node-outline ((node org-roam-node))
      ;; `outline-display' is a list of each headline path in the outline
      ;; we display. We initialize it to the full path and then remove
      ;; elements as needed.
      (let* ((outline-path (append (org-roam-node-olp node)
                                   `(,(org-roam-node-title node))))
             (level (org-roam-node-level node))
             (outline-display outline-path)
             (tags-width 15)
             (path-width (- (window-width) mh//org-roam-helm-tags-width 1)))
        ;; if the current node is not the file-level node, append the file
        ;; level node to `outline-display', which otherwise isn't part of
        ;; the outline path.
        (if (> level 0)
            (let* ((file (org-roam-node-file node))
                   (title (car (org-roam-db-query
                                [:select title :from nodes
                                 :where (and (= file $s1)
                                             (= level 0))]
                                file))))
              (setq outline-display (append title outline-display))))
        ;; `(length outline-display)' computes the string length of all
        ;; separators. 2 computes the maximum difference between the
        ;; string length of '...' and a headline string, in case on
        ;; headline is shorter than 3 chars.
        (while (and (>= (+ (-sum (cl-map 'list 'length outline-display))
                           (length outline-display)
                           2)
                        path-width)
                    ;; Don't remove the first or last headline path. Deal
                    ;; with this case later.
                    (>= (length outline-display) 2))
          (setq outline-display (-remove-at 1 outline-display)))
        ;; Remove the first headline path if the first and last
        ;; collectively exceed `path-width'.
        (if (>= (+ (-sum (cl-map 'list 'length outline-display))
                   (length outline-display)
                   2)
                path-width)
            (setq outline-display (-remove-at 0 outline-display)))
        (let ((outline-string (car outline-display)))
          ;; The total headline path exceeded the max width, so we cut out
          ;; one or more path elements.
          (if (< (length outline-display)
                 (length outline-path))
              (if (eq (length outline-display) 1)
                  (concat ".../" outline-string)
                (setq outline-string (concat outline-string "/..."))))
          (let ((index 1))
            (while (< index (length outline-display))
	      (setq outline-string (concat outline-string "/"
                                           (nth index outline-display)))
              (setq index (+ 1 index))))
          outline-string)))

    (setq org-roam-node-display-template
          (concat "${outline:"
                  (number-to-string (- (window-width) mh//org-roam-helm-tags-width 1))
                  "} ${tags:"
                  (number-to-string mh//org-roam-helm-tags-width)
                  "}")))

  (use-package org-roam-bibtex
    :config
    (setq orb-file-field-extensions nil)
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
