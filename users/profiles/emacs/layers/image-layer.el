;;; image-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def image
  :setup
  (use-package image-mode)

  :postsetup
  (:layer modal
   (general-define-key
    :keymaps 'image-map
    "i" 'image-increase-size
    "o" 'image-decrease-size
    "l" 'image-next-frame
    "h" 'image-previous-frame)))

;;; image-layer.el ends here
