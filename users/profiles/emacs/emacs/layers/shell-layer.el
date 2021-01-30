;;; shell-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def shell
  :depends (programming)

  :presetup
  (:layer straight
   (straight-use-package 'vterm-toggle)
   (straight-use-package 'bash-completion)
   (straight-use-package 'fish-completion)
   ;; use nixos instead of straight to download/install vterm
   (straight-use-package '(vterm :type built-in)))

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
    ;; :init
    ;; (add-hook 'eshell-mode-hook (lambda ()
    ;;                               (general-define-key
    ;;                                :keymaps 'company-active-map
    ;;                                "<tab>" 'company-complete-common)) 0 t)
    :config
    ;; TODO not sure if this works
    (setq eshell-buffer-maximum-lines 0))

  ;; Don't truncate terminal output
  (setq term-buffer-maximum-size 0)

  ;; (use-package eterm-256color
  ;;   :hook (vterm-mode . eterm-256color-mode))

  (use-package vterm
    :config
    (setq vterm-shell "/run/current-system/sw/bin/fish")
    (setq vterm-toggle-fullscreen-p nil)
    ;; increase max number of scrollback lines. TODO this apparently
    ;; only works up to 1e5 and must be modified in source. That is a
    ;; bad approach and should be modified upstream.
    (setq vterm-max-scrollback (round 1e9))
    ;; kill vterm buffers when process terminated
    (setq vterm-kill-buffer-on-exit t)
    (add-to-list 'display-buffer-alist
                 '("vterm"
                   (display-buffer-reuse-window display-buffer-same-window))))

  (defun mh/run-ipython ()
    (interactive)
    (call-interactively 'vterm)
    (vterm-send-string "ipython")
    (vterm-send-return))

  (use-package vterm-toggle)

  :postsetup
  (:layer modal
   (defun evil-collection-vterm-escape-stay ()
     "Go back to normal state but don't move cursor backwards.
Moving cursor backwards is the default vim behavior but
it is not appropriate in some cases like terminals."
     (setq-local evil-move-cursor-back nil))
   (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)

   (general-define-key
    :states '(normal insert)
    :keymaps 'vterm-mode-map
    "C-k" 'vterm-send-up
    "C-j" 'vterm-send-down
    ;; unbind clear since I'm too used to using C-l for centering the
    ;; page
    "C-l" 'recenter-top-bottom)

   (localleader :keymaps 'vterm-mode-map
     "t" 'vterm-copy-mode
     "c" 'vterm-clear-scrollback)

   ;; TODO evil interferes with this
   (localleader :keymaps 'vterm-copy-mode-map
     "t" 'vterm-copy-mode-done)

   (general-def mh/prefix-shell-map
     "c" 'async-shell-command
     "e" 'eshell
     "t" 'vterm-toggle
     "T" 'vterm
     "p" 'mh/run-ipython)

   (general-define-key
    :keymaps 'comint-mode-map
    "C-k" 'comint-previous-input
    "C-j" 'comint-next-input
    "C-r" 'comint-history-isearch-backward-regexp)

   (general-define-key
    :states '(normal insert)
    :keymaps '(eshell-hist-mode-map)
    "C-k" 'eshell-previous-matching-input-from-input
    "C-j" 'eshell-next-matching-input-from-input)

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

  ;; (:layer (modal completions)
  ;;  (add-hook 'eshell-mode-hook (lambda ()
  ;;                                (general-define-key
  ;;                                 :keymaps 'local
  ;;                                 "<tab>" 'company-complete-common))))

  (:layer flycheck
   (add-hook 'sh-mode-hook 'flycheck-mode)
   ;; (setq flycheck-checker 'lsp-ui)
   ;; (flycheck-add-next-checker 'lsp-ui 'sh-posix-bash)
   )

  (:layer lsp
   (add-hook 'sh-mode-hook 'lsp)))

;;; shell-layer.el ends here
