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
    "h" 'image-previous-frame))

  :func
  (defun mh/svg-replace-black-with-current-color ()
    "Replace black strokes and fills with currentColor in current SVG document."
    (interactive)
    (replace-string "stroke:#000000" "stroke:currentColor")
    (replace-string "fill:#000000" "fill:currentColor")))

;;; image-layer.el ends here
