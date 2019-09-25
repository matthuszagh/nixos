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
   ))

;;; lsp-layer.el ends here
