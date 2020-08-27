;;; lisp-layer.el --- Lisp Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def lisp
  :presetup
  (:layer straight
   (straight-use-package 'lispy)
   (straight-use-package 'lispyville))

  :setup
  (use-package lispy
    :config
    (add-hook 'emacs-lisp-mode-hook (lambda ()
                                      (lispy-mode 1))))

  (use-package lispyville
    :config
    (add-hook 'lispy-mode-hook #'lispyville-mode)))

;;; lisp-layer.el ends here
