;;; image-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def image
  :setup
  (use-package image-mode)

  ;; :postsetup
  ;; (:layer modal
  ;;         (localleader :keymaps 'image-mode-map
  ;;           "=" 'image-increase-size
  ;;           "-" 'image-decrease-size))
  )

;;; image-layer.el ends here
