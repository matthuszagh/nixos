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
               (remove-hook 'before-save-hook 'clang-format-buffer t))))

  :func
  (defun mh/color-name-to-asy-rgb (name)
    ""
    (let ((res "rgb(")
          (color-list (color-name-to-rgb name)))
      (setq res (concat res (number-to-string (nth 0 color-list)) ", "))
      (setq res (concat res (number-to-string (nth 1 color-list)) ", "))
      (setq res (concat res (number-to-string (nth 2 color-list)) ")")))))

;;; asy-layer.el ends here
