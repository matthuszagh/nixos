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
    (setq rustic-lsp-server 'rls)
    (add-hook 'rustic-mode-hook
              (lambda ()
                (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p t)))
    (setq rustic-format-on-save t)))

;;; rust-layer.el ends here
