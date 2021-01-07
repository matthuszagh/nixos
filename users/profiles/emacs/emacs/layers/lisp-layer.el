;;; lisp-layer.el --- Lisp Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def lisp
  :presetup
  (:layer straight
   (straight-use-package 'lispy)
   (straight-use-package 'lispyville))

  :setup
  (use-package lispy)

  (use-package lispyville
    :config
    (add-hook 'lispy-mode-hook #'lispyville-mode)
    (lispyville-set-key-theme '(operators c-w additional)))

  :postsetup
  (:layer elisp
   (add-hook 'emacs-lisp-mode-hook (lambda ()
		                     (lispy-mode 1)))))

;;; lisp-layer.el ends here
