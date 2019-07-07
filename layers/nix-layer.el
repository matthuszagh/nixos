;;; nix-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def nix
  :depends (programming)

  :presetup
  (:layer straight
          (straight-use-package 'nix-mode)
          (straight-use-package 'nix-update))

  :setup
  (use-package nix-mode
    :mode "\\.nix\\'"
    :hook
    ((nix-repl-mode . (lambda ()
                        (setq-local fill-column nil)))))
  (use-package nix-update))

;;; nix-layer.el ends here
