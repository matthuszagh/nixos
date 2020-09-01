;;; asy-layer.el --- Asymptote Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def asy
  :setup
  (use-package asy-mode
    :mode (("\\.asy\\'" . asy-mode)))

  :postsetup
  (:layer c
   ;; asy-mode apparently inherits from c-mode-common so we must
   ;; disable clang formatting.
   (add-hook 'asy-mode-hook
             (lambda ()
               (remove-hook 'before-save-hook 'clang-format-buffer t)))))

;;; asy-layer.el ends here
