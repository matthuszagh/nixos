;;; snippet-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def snippet
  :setup
  (use-package yasnippet
    :config
    ;; Allow nested expansion.
    (setq yas-triggers-in-field t)
    (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets/"))
    (yas-reload-all)
    ;; auto expand snippets with # condition: 'auto
    (defun mh/yas-try-expanding-auto-snippets ()
      (when (and (boundp 'yas-minor-mode) yas-minor-mode)
        (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
          (yas-expand))))
    (add-hook 'post-command-hook #'mh/yas-try-expanding-auto-snippets))

  ;; (use-package yasnippet-snippets
  ;;   :config
  ;;   (yas-reload-all))

  :postsetup
  (:layer TeX
   (add-hook 'latex-mode-hook #'yas-minor-mode))

  (:layer org
   (add-hook 'org-mode-hook #'yas-minor-mode))

  (:layer verilog
   (add-hook 'verilog-mode-hook #'yas-minor-mode))

  (:layer elisp
   (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode))

  (:layer c
   (add-hook 'c-mode-common-hook #'yas-minor-mode)))

;;; snippet-layer.el ends here
