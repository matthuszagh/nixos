;;; nix-layer.el --- Summary -*-lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def nix
  :depends (programming)

  :presetup
  (:layer straight
   (straight-use-package 'nix-mode)
   (straight-use-package 'direnv)
   (straight-use-package 'nix-update)
   (straight-use-package 'nixpkgs-fmt))

  :setup
  (use-package nix-mode
    :mode "\\.nix\\'"
    :hook
    ((nix-repl-mode . (lambda ()
                        (setq-local fill-column nil))))
    :custom
    ;; set this to indent-relative if issues occur.
    (nix-indent-function #'nix-indent-line))

  (add-to-list 'display-buffer-alist
               '("\\*nixos-rebuild\\*"
                 (display-buffer-reuse-window display-buffer-same-window)))

  (use-package nix-update)

  (use-package direnv
    :config
    (direnv-mode)
    ;; Inhibits the summary unless an update-environment call is
    ;; made. The summary is annoying because it shifts the buffer
    ;; contents. This does not stop direnv updating the environment.
    (setq direnv-always-show-summary nil))

  (use-package nixpkgs-fmt
    :config
    (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode))

  (defun mh/nix-rebuild ()
    (interactive)
    (command-execute
     (async-shell-command "cd ~/src/nixos/ && make" "*nixos-rebuild*")))

  (defun mh/nix-rebuild-show-trace ()
    (interactive)
    (command-execute
     (async-shell-command "cd ~/src/nixos/ && make trace" "*nixos-rebuild*")))

  :postsetup
  (:layer modal
   (general-def mh/prefix-system-map
     "r" 'mh/nix-rebuild
     "R" 'mh/nix-rebuild-show-trace))

  (:layer (modal dumb-jump)
   (localleader 'nix-mode-map
     "d" 'dumb-jump-go)))

;;; nix-layer.el ends here
