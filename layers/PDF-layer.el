;;; pdf-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def pdf
  :presetup
  (:layer straight
   (straight-use-package 'pdf-tools))

  :setup
  ;; TODO setup mouseless link clicking. Apparently, pdf-tools can
  ;; find links and present them as key sequences in the same way
  ;; vimium does. See pdf-links-action-perform. Currently imagemagick
  ;; is spitting out errors related to fonts. Fix this and set it up.

  ;; TODO change search keybinding.
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
     "SPC" 'mh/command-prefix)

   (general-define-key
    :state 'normal
    :keymaps 'pdf-outline-buffer-mode-map
    "<tab>" 'outline-toggle-children)

   (localleader :keymaps 'pdf-view-mode-map
     "s" 'pdf-occur
     "g" 'pdf-view-goto-page)))

;;; pdf-layer.el ends here
