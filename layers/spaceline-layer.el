;;; spaceline-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

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
      (spaceline-spacemacs-theme)))

  :postsetup
  (:layer helm
   (spaceline-helm-mode))

  (:layer documentation
   (spaceline-info-mode)))

;;; spaceline-layer.el ends here
