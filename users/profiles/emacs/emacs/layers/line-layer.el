;;; line-layer.el --- Line Layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Text reflow and wrapping functionality.

;;; Code:

(layer-def line
  :presetup
  (:layer straight
   (straight-use-package 'visual-fill-column))

  :setup
  (add-hook 'text-mode-hook
            (lambda ()
              ;; TeX modes are derived from text-mode, but we prefer
              ;; to treat TeX-mode like a programming mode.
              (if (not TeX-mode-p)
                  (visual-line-mode))))

  (use-package visual-fill-column
    :hook
    ((visual-line-mode . visual-fill-column-mode))
    :config
    ;; (add-hook 'markdown-mode-hook #'visual-fill-column-mode)
    (setq visual-fill-column-width 70)))

;;; line-layer.el ends here
