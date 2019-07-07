;;; epub-layer.el -*-lexical-binding: t; -*-

;;; Code:

(layer-def epub
  :presetup
  (:layer straight
          (straight-use-package 'nov))

  :setup
  (use-package nov
    :after (dash esxml)
    :mode "\\.epub\\'"
    :init
    (defun my-nov-font-setup ()
      (face-remap-add-relative 'variable-pitch :family "San Francisco Text"
                               :height 1.0))
    :hook ((nov-mode . my-nov-font-setup))
    :config
    (setq nov-text-width 70)))

;;; epub-layer.el ends here
