;;; line-layer.el --- Line Layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Text reflow and wrapping functionality.

;;; Code:

(layer-def line
  :setup
  (add-hook 'text-mode-hook
            (lambda ()
              ;; TeX modes are derived from text-mode, but we prefer
              ;; to treat TeX-mode like a programming mode.
              (if (not TeX-mode-p)
                  (visual-line-mode)))))

;;; line-layer.el ends here
