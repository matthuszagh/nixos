;;; rust-layer.el --- Rust Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def rust
  :presetup
  (:layer straight
   (straight-use-package 'rustic))

  :setup
  (use-package rustic
    :config
    (setq rustic-lsp-server 'rls)))

;;; rust-layer.el ends here
