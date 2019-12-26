;;; naysayer-theme-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def naysayer-theme
  :depends (base no-littering)

  :setup
  (use-package naysayer-theme
    :config
    (load-theme 'naysayer t))

  :postsetup
  (:layer org
   (set-face-foreground 'org-block (face-foreground 'default))

   ;; set inline code appearance
   (set-face-background 'org-code "unspecified")
   (set-face-foreground 'org-code "#8686ae")
   (set-face-foreground 'org-link "#86aed5"))
  )

;;; naysayer-theme-layer.el ends here
