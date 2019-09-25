;;; default-theme-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def default-theme
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
   ;;(face-remap-add-relative 'org-block `(:foreground ,(face-foreground 'default)))
   ))


;;; default-theme-layer.el ends here
