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
    ;; apply default snippets
    (apply #'aas-set-snippets 'org-mode laas-basic-snippets)
    (apply #'aas-set-snippets 'org-mode laas-subscript-snippets)
    (apply #'aas-set-snippets 'org-mode laas-accent-snippets)

    ;; expand "//" into frac
    ;; TODO doesn't work in latex-mode since '/' already defined there.
    (let ((frac-snippet
           (list
            :cond #'(lambda ()
                      (and (texmathp)
                           (aas-object-on-left-condition)))
            :expansion-desc "Wrap object on the left with \\frac{}{}, leave `point' in the denuminator."
            "//" #'laas-smart-fraction)))
      (dolist (mode '(org-mode))
        (apply #'aas-set-snippets mode frac-snippet)))

    ;; custom math snippets
    (let ((math-snippets
           (list
            :cond #'texmathp
            "cs" (lambda ()
              	   (interactive)
                   (yas-expand-snippet (concat "\\begin{cases}\n"
                                               "    $1\n"
                                               "  \\end{cases}$0")))
            "mt" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\begin{bmatrix}\n"
                                               "    $1\n"
                                               "  \\end{bmatrix}$0")))
            "dt" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\begin{vmatrix}\n"
                                               "    $1\n"
                                               "  \\end{vmatrix}$0")))
            "bf" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\mathbf{$1}$0")))
            "bb" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\mathbb{$1}$0")))
            "tx" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\text{$1}$0")))
            "rm" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\mathrm{$1}$0"))))))
      (dolist (mode '(latex-mode org-mode))
        (apply #'aas-set-snippets mode math-snippets))))

  :postsetup
  (:layer TeX
   (add-hook 'latex-mode-hook #'yas-minor-mode))

  (:layer org
   (add-hook 'org-mode-hook #'yas-minor-mode)
   (let ((latex-block-snippets
          (list
           :cond (lambda ()
                   (and (mh/org-in-latex-blockp)
                        (mh/point-at-line-begp)))
           "eqn" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\begin{equation}\\tag{0}\n"
                                               "  $0\n"
                                               "\\end{equation}")))
           "aln" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\begin{align}\n"
                                               "  $0\n"
                                               "\\end{align}")))
           "prf" (lambda ()
                   (interactive)
                   (yas-expand-snippet (concat "\\begin{proof}\n"
                                               "  \\begin{align}\n"
                                               "    $1\n"
                                               "  \\end{align}\n"
                                               "\\end{proof}$0"))))))
     (apply #'aas-set-snippets 'org-mode latex-block-snippets)))

  (:layer verilog
   (add-hook 'verilog-mode-hook #'yas-minor-mode))

  (:layer elisp
   (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode))

  (:layer c
   (add-hook 'c-mode-common-hook #'yas-minor-mode)))

;;; snippet-layer.el ends here
