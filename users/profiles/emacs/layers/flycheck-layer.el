;;; flycheck-layer.el --- Flycheck Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def flycheck
  :depends (programming)

  :presetup
  (:layer straight
   (straight-use-package 'flycheck))

  :setup
  (use-package flycheck
    :functions global-flycheck-mode
    :config
    ;; Turn flycheck on everywhere
    (global-flycheck-mode t))

  :postsetup
  (:layer modal
   (general-def mh/prefix-prog-map
     "l" 'flycheck-list-errors))

  ;; (:layer (lsp python)
  ;;  ;; (flycheck-add-next-checker 'lsp 'python-mypy)
  ;;  ;; (flycheck-add-next-checker 'python-mypy 'python-flake8)
  ;;  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)
  ;;  )
  )

;;; flycheck-layer.el ends here
