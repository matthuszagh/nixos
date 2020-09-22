;;; lsp-layer.el --- Language Server Protocol Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def lsp
  :presetup
  (:layer straight
   (straight-use-package 'lsp-mode)
   (straight-use-package 'lsp-ui)
   (straight-use-package '(lsp-pyright :host github :repo "emacs-lsp/lsp-pyright")))

  :setup
  (use-package lsp-mode
    :hook (prog-mode . lsp)
    :commands lsp
    :config
    (setq lsp-enable-snippet nil)
    (require 'lsp-clients)
    (setq lsp-completion-provider :capf)
    (add-to-list 'lsp-language-id-configuration '(cython-mode . "cython"))
    (setq lsp-log-io t)
    ;; automatically guess project root with projectile
    (setq lsp-auto-guess-root t)
    (setq lsp-idle-delay 0.5))

  (use-package lsp-ui
    :config
    (lsp-ui-doc-enable 1))

  :postsetup
  (:layer default-theme
   (set-face-attribute 'lsp-ui-sideline-symbol-info nil :foreground "#5c5d56" :height 0.8)
   (set-face-attribute 'lsp-ui-sideline-symbol nil :height 0.8)
   (set-face-attribute 'lsp-ui-sideline-current-symbol nil :height 0.8))

  (:layer modal
   (general-define-key
    :states 'normal
    "g r" 'lsp-ui-peek-find-references
    "g i" 'lsp-find-implementation
    "g x" 'lsp-rename
    "g s" 'lsp-ui-find-workspace-symbol)

   (general-define-key
    :keymaps 'lsp-ui-peek-mode-map
    "j" 'lsp-ui-peek--select-next
    "k" 'lsp-ui-peek--select-prev))

  (:layer python
   (use-package lsp-pyright
     :hook (python-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp)))))
  (:layer nix
   (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
   (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                     :major-modes '(nix-mode)
                     :server-id 'nix))))

;;; lsp-layer.el ends here
