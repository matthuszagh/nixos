;;; flycheck-layer.el --- Flycheck Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def flycheck
  :depends (programming)

  :setup
  (use-package flycheck
    :functions global-flycheck-mode
    :config
    ;; Turn flycheck on everywhere
    (global-flycheck-mode t)
    ;; TODO none of this should be here.
    ;; ;; There are issues with company mode and flycheck in terminal mode.
    ;; ;; This is outlined at:
    ;; ;; https://github.com/abingham/emacs-ycmd
    ;; (when (not (display-graphic-p))
    ;;   (setq flycheck-indication-mode nil))
    ;; (setq flycheck-clang-language-standard nil)
    )

  :postsetup
  (:layer modal
   (general-def mh/prefix-prog-map
     "l" 'flycheck-list-errors)))

;;; flycheck-layer.el ends here
