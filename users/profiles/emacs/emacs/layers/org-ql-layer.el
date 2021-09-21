;;; org-ql-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org-ql
  :presetup
  (:layer straight
   (straight-use-package 'org-ql))

  :setup
  (use-package org-ql)
  (use-package helm-org-ql
    :config
    (setq helm-org-ql-reverse-paths nil)
    ;; don't display filename in query results
    (defun mh//helm-org-ql--heading (window-width)
      "Return string for Helm for heading at point.
WINDOW-WIDTH should be the width of the Helm window."
      (font-lock-ensure (point-at-bol) (point-at-eol))
      (let* ((width window-width)
             (path (-> (org-get-outline-path)
                       (org-format-outline-path width nil "")
                       (org-split-string "")))
             (heading (org-get-heading t))
             (path (if helm-org-ql-reverse-paths
                       (concat heading "\\" (s-join "\\" (nreverse path)))
                     (concat (s-join "/" path) "/" heading))))
        (cons path (point-marker))))
    (advice-add 'helm-org-ql--heading :override #'mh//helm-org-ql--heading))

  :postsetup
  (:layer (helm org modal)
   (cl-defun mh//helm-org-ql-olp-source (buffers-files &key (name "helm-org-ql-olp"))
     (helm-make-source name 'helm-source-sync
       :candidates (lambda nil
                     (let* ((query (->> helm-pattern
                                        s-split-words
                                        (apply #'list 'olp)))
                            (window-width (window-width (helm-window))))
                       (when query
                         (with-current-buffer (helm-buffer-get)
                           (setq helm-org-ql-buffers-files buffers-files))
                         (ignore-errors
                           ;; Ignore errors that might be caused by partially typed queries.

                           ;; FIXME: This doesn't prevent warnings that are errors occurring during
                           ;; byte-compilation due to partially typed values which can't be correctly
                           ;; pre-processed, e.g. "ts:to=2019-01-0", which can't be parsed into a
                           ;; timestamp.  A "*Compile-Log*" buffer is displayed with "Error: Wrong type
                           ;; argument: integerp, nil".  With my Helm settings, it's hidden as soon as
                           ;; the query is typed correctly, so it's tolerable, but I'd prefer to fix it.
                           ;; I haven't found a way to ignore the error/warning; `with-no-warnings' has
                           ;; no effect, and we're already using `ignore-errors'.  The only solution I
                           ;; can think of would be to ignore the errors/warnings higher up the chain
                           ;; where byte-compilation is actually done, but it might not be a good idea
                           ;; to always ignore such errors/warnings.
                           (org-ql-select buffers-files query
                             :action `(helm-org-ql--heading ,window-width))))))
       :match #'identity
       :fuzzy-match nil
       :multimatch nil
       :nohighlight t
       :volatile t
       :keymap helm-org-ql-map
       :action helm-org-ql-actions))

   (setq mh-org-wiki-file "~/doc/notes/wiki.org")
   (defun mh/helm-global-search ()
     (interactive)
     (helm :sources `(,(mh//helm-org-ql-olp-source (list mh-org-wiki-file))
                      ,(helm-def-source--info-files)
                      helm-source-man-pages
                      helm-source-recoll-library)))

   (defun mh/helm-global-search ()
     (interactive)
     (helm :sources `(,(helm-org-ql-source (list mh-org-wiki-file))
                      ,(helm-def-source--info-files)
                      helm-source-man-pages
                      helm-source-recoll-library)))

   (general-def mh/prefix-search-map
     "g" 'mh/helm-global-search)

   (general-def helm-org-ql-map
     "C-j" 'helm-next-line
     "C-k" 'helm-previous-line)))

;;; org-ql-layer.el ends here
