;;; rust-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def rust
  :presetup
  (:layer straight
   (straight-use-package 'rust-mode))

  :setup
  (use-package rust-mode))

;;; rust-layer.el ends here
