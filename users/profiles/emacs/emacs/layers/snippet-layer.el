;;; snippet-layer.el --- Snippet Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def snippet
  :presetup
  (:layer straight
   (straight-use-package '(yasnippet :type git :host github :repo "Zetagon/yasnippet"))
   (straight-use-package '(auto-activating-snippets :type git :host github :repo "ymarco/auto-activating-snippets"))
   (straight-use-package '(LaTeX-auto-activating-snippets :type git :host github :repo "tecosaur/LaTeX-auto-activating-snippets")))

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

  (use-package auto-activating-snippets
    :hook (LaTeX-mode . auto-activating-snippets-mode)
    :hook (org-mode . auto-activating-snippets-mode)
    :config
    (add-hook 'org-src-mode-hook
              (lambda ()
                (if (eq major-mode 'latex-mode)
                    (auto-activating-snippets-mode)))))

  (use-package latex-auto-activating-snippets
    :after latex
    :config
    (apply #'aas-set-snippets 'org-mode laas-basic-snippets)
    (apply #'aas-set-snippets 'org-mode laas-subscript-snippets)
    (apply #'aas-set-snippets 'org-mode laas-frac-snippet)
    (apply #'aas-set-snippets 'org-mode laas-accent-snippets))

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
