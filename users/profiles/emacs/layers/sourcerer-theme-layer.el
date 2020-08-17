;;; sourcerer-theme-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def sourcerer-theme
  :depends (base no-littering)
  :presetup
  (:layer straight
   (straight-use-package 'sourcerer-theme))

  :setup
  (use-package sourcerer-theme
    :config
    (load-theme 'sourcerer)
    (add-to-list 'default-frame-alist '(cursor-color . "#c2c2b0"))
    (add-to-list 'default-frame-alist '(hl-line-face . "gray16")))

  :postsetup
  (:layer org
   (set-face-foreground 'org-block (face-foreground 'default))

   ;; set inline code appearance
   (set-face-background 'org-code "unspecified")
   (set-face-foreground 'org-code "#8686ae")
   (set-face-foreground 'org-link "#86aed5")

   ;;(face-remap-add-relative 'org-block `(:foreground ,(face-foreground 'default)))
   )

  (:layer default-mode-line
   (set-face-foreground 'sml/global "#c2c2b0")
   (set-face-foreground 'sml/filename "gold")))


;;; sourcerer-theme-layer.el ends here
