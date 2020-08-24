;;; epub-layer.el --- EPUB Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def epub
  :presetup
  (:layer straight
   (straight-use-package 'nov))

  :setup
  (use-package nov
    :mode ("\\.epub\\'" . nov-mode)
    :init
    (defun mh/nov-font-setup ()
      (face-remap-add-relative 'variable-pitch :family "Source Code Pro"
                               :height 1.0))
    :hook ((nov-mode . mh/nov-font-setup))
    :config
    (setq nov-text-width 70)

    ;; TODO get full justification working
    (defun mh/nov-window-configuration-change-hook ()
      (mh/nov-post-html-render-hook)
      (remove-hook 'window-configuration-change-hook
                   'mh/nov-window-configuration-change-hook
                   t))

    (defun mh/nov-post-html-render-hook ()
      (if (get-buffer-window)
          (let ((max-width (pj-line-width))
                buffer-read-only)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (not (looking-at "^[[:space:]]*$"))
                  (goto-char (line-end-position))
                  (when (> (shr-pixel-column) max-width)
                    (goto-char (line-beginning-position))
                    (pj-justify)))
                (forward-line 1))))
        (add-hook 'window-configuration-change-hook
                  'mh/nov-window-configuration-change-hook
                  nil t)))
    (add-hook 'nov-post-html-render-hook 'mh/nov-post-html-render-hook)))

;;; epub-layer.el ends here
