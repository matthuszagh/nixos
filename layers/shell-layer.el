;;; shell-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def shell
  :depends (programming)

  :setup
  (use-package shell
    :hook
    ((shell-mode . (lambda ()
                     (setq-local scroll-margin 0)
                     (setq-local scroll-conservatively 101))))
    :config
    (add-to-list 'same-window-buffer-names "*shell*")
    ;; (setq explicit-shell-file-name "/home/matt/.nix-profile/bin/fish")
    (setq explicit-shell-file-name nil)
    )

  (use-package eshell
    :hook
    ((eshell-mode . (lambda ()
                      (setq-local scroll-margin 0)
                      (setq-local scroll-conservatively 101))))
    :config
    ;; TODO not sure if this works
    (setq eshell-buffer-maximum-lines 0))

  ;; Don't truncate terminal output
  (setq term-buffer-maximum-size 0)

  :postsetup
  (:layer modal
   (general-def mh/prefix-shell-map
     "c" 'async-shell-command
     "e" 'eshell
     "t" 'term)

   (localleader :keymaps 'term-line-mode
     "c" 'comint-clear-buffer))

  (:layer completions
   (use-package bash-completion
     :config
     (bash-completion-setup))
   (use-package fish-completion
     :config
     (global-fish-completion-mode)
     (setq fish-completion-fallback-on-bash-p t)))

  (:layer flycheck
   (add-hook 'sh-mode-hook 'flycheck-mode)
   (setq flycheck-checker 'lsp-ui)
   (flycheck-add-next-checker 'lsp-ui 'sh-posix-bash))

  (:layer lsp
   (add-hook 'sh-mode-hook 'lsp)))

;;; shell-layer.el ends here
