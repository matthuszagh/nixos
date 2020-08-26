;;; lsp-layer.el --- Language Server Protocol Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def lsp
  :presetup
  (:layer straight
   (straight-use-package 'lsp-mode)
   (straight-use-package 'lsp-ui)
   (straight-use-package 'company-lsp)
   (straight-use-package '(lsp-pyright :host github :repo "emacs-lsp/lsp-pyright")))

  :setup
  (use-package lsp-mode
    :hook (prog-mode . lsp)
    :commands lsp
    :config
    (setq lsp-enable-snippet nil)
    (require 'lsp-clients)
    (add-to-list 'lsp-language-id-configuration '(cython-mode . "cython"))
    (setq lsp-log-io t)
    ;; automatically guess project root with projectile
    (setq lsp-auto-guess-root t))

  (use-package lsp-ui
    :demand t
    :hook ((lsp-mode . lsp-ui-mode)
           (lsp-ui-mode . lsp-ui-peek-mode)
           (lsp-ui-mode . lsp-ui-doc-mode))
    :commands lsp-ui-mode
    :config
    ;;(setq lsp-ui-doc-use-webkit t)
    )

  (use-package company-lsp
    :commands company-lsp
    :config
    (add-to-list 'company-backends 'company-lsp))

  :postsetup
  (:layer flycheck
   (use-package lsp-ui-flycheck
     :hook (lsp-after-open . (lambda ()
                               lsp-ui-flycheck-enable 1)))
   (require 'lsp-ui-flycheck)
   (with-eval-after-load 'lsp-mode
     (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))
   ;; (add-to-list 'flycheck-checkers 'lsp-ui)
   )

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
                            (lsp))))))

;;; lsp-layer.el ends here
