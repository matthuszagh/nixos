;;; diff-layer.el --- Diff Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def diff
  :setup
  (add-hook 'diff-mode-hook
            (lambda ()
              (setq-local require-final-newline nil))))

;;; diff-layer.el ends here
