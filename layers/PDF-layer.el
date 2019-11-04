;;; pdf-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def pdf
  :presetup
  (:layer straight
   (straight-use-package 'pdf-tools))

  :setup
  (use-package pdf-tools
    :demand t
    :mode "\\.pdf\\'"
    :init
    ;; Only warn when opening files larger than 1GB. This is particularly useful for PDFs which are
    ;; often larger than the default threshold.
    (setq large-file-warning-threshold nil)
    :config
    (pdf-tools-install)
    ;; fixes an issue in emacs 27 where pdf is blurry and too large otherwise.
    (setq image-scaling-factor 1)
    (setq-default pdf-view-display-size 'fit-page))

  :postsetup
  (:layer modal
   (general-def pdf-view-mode-map
     "j" 'pdf-view-next-line-or-next-page
     "k" 'pdf-view-previous-line-or-previous-page
     "SPC" 'mh/command-prefix)))

;;; pdf-layer.el ends here
