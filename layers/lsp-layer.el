;;; lsp-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def lsp
  :setup
  (use-package lsp-mode
    :hook (prog-mode . lsp)
    :commands lsp
    :config
    (setq lsp-enable-snippet nil)
    (require 'lsp-clients)
    (setq lsp-log-io t)
    ;; automatically guess project root with projectile
    (setq lsp-auto-guess-root t))

  (use-package lsp-ui
    :demand t
    :hook (lsp-mode . lsp-ui-mode)
    :commands lsp-ui-mode)

  (use-package company-lsp
    :commands company-lsp
    :config
    (add-to-list 'company-backends 'company-lsp ))

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
    "g r" 'lsp-find-references
    "g i" 'lsp-find-implementation)))

;;; lsp-layer.el ends here
