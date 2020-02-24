;;; naysayer-theme-layer.el --- Naysayer Theme -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def naysayer-theme
  :depends (base no-littering)

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
   (set-face-background 'org-code "unspecified")
   (set-face-foreground 'org-code "#8686ae")
   (set-face-foreground 'org-link "#86aed5")

   ;; org todo keyword faces
   (set-face-foreground 'org-todo "#d4d4d4")
   (setq org-todo-keyword-faces '(("TODO" . (:foreground "Pink"
                                             :weight bold))))))

;;; naysayer-theme-layer.el ends here
