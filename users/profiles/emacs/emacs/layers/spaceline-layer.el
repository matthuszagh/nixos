;;; spaceline-layer.el --- Spaceline Mode Line Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def spaceline
  :depends (base no-littering)
  :presetup
  (:layer straight
   (straight-use-package 'spaceline))

  :setup
  (use-package spaceline
    :config
    (use-package spaceline-segments)
    (use-package spaceline-config
      :config
      (spaceline-spacemacs-theme))
    (setq spaceline-minor-modes-p nil))

  :postsetup
  (:layer helm
   (spaceline-helm-mode))

  (:layer modal
   ;; Change evil face depending on active evil mode.
   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

  (:layer org
   (setq spaceline-org-clock-format-function 'mh/org-get-truncated-clock-string)))

;;; spaceline-layer.el ends here
