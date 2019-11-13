;;; programming-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def programming
  :setup
  (use-package prog-mode
    :hook
    ((prog-mode . display-line-numbers-mode)
     (prog-mode . (lambda ()
                    ;; Highlighting in cmake-mode this way interferes with
                    ;; cmake-font-lock, which is something I don't yet understand.
                    (when (not (derived-mode-p 'cmake-mode))
                      (font-lock-add-keywords
                       nil
                       '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                          1 font-lock-warning-face t))))))
     ;; Disable auto-fill.
     (prog-mode . (lambda () (auto-fill-mode -1)))))

  (use-package banner-comment))

;;; programming-layer.el ends here
