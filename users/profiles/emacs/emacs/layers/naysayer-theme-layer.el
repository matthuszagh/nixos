;;; naysayer-theme-layer.el --- Naysayer Theme -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def naysayer-theme
  :depends (base no-littering)

  :presetup
  (:layer straight
   (straight-use-package 'naysayer-theme))

  :setup
  (use-package naysayer-theme
    :config
    (load-theme 'naysayer t)
    (setq mh-cursor-color (face-background 'cursor))
    (setq mh-foreground-color (face-foreground 'default))
    (setq mh-background-color (face-background 'default)))

  :postsetup
  (:layer org
   (set-face-foreground 'org-block (face-foreground 'default))

   ;; set inline code appearance
   (set-face-background 'org-code "#113e47")
   (set-face-foreground 'org-code (face-foreground 'default))
   (set-face-foreground 'org-link "#86aed5")

   ;; org todo keyword faces
   (set-face-foreground 'org-todo "#d4d4d4")
   (setq org-todo-keyword-faces '(("TODO" . (:foreground "Pink"
                                             :weight bold)))))
  (:layer org-ref
   (set-face-foreground 'org-ref-ref-face "#ffaa00"))

  (:layer helm
   (set-face-attribute 'helm-ff-directory nil :foreground "white" :background (face-background 'default))
   (set-face-attribute 'helm-selection nil :background "blue")
   (let ((fcolor (face-foreground 'default)))
     (set-face-attribute 'helm-ff-file-extension nil :foreground fcolor)
     (set-face-attribute 'helm-ff-file nil :foreground fcolor))))

;;; naysayer-theme-layer.el ends here
