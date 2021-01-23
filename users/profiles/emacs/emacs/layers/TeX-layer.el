;;; tex-layer.el --- TeX Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def tex
  :presetup
  (:layer straight
   (straight-use-package 'auctex)
   (straight-use-package 'auctex-latexmk))

  :setup
  (use-package tex-site
    :demand t
    :init
    (defun my-add-auctex-file-variables ()
      (interactive)
      (if (and (not buffer-read-only)
               (string= (file-name-extension (buffer-file-name)) "tex"))
          (progn
            (add-file-local-variable 'coding 'utf-8)
            (goto-char (point-min)))))

    (defun latex-indent ()
      "Run latexindent on the current buffer."
      (interactive)
      (with-no-warnings
        (shell-command (concat "latexindent " (buffer-file-name) " > " (buffer-file-name) ".tmp && mv "
                               (buffer-file-name) ".tmp " (buffer-file-name)))))

    ;; ;; When we byte-compile we need to have the autoloads loaded in order to
    ;; ;; properly get auctex working, otherwise auctex is not loaded correctly
    ;; (load "auctex-autoloads" nil t)
    :hook ((LaTeX-mode . TeX-source-correlate-mode)
           (LaTeX-mode . auto-fill-mode)
           (LaTeX-mode . flyspell-mode)
           (LaTeX-mode . flyspell-buffer)
           (LaTeX-mode . (lambda ()
                           (TeX-fold-mode 0)
                           (add-hook 'find-file-hook 'TeX-fold-buffer t t)
                           (add-hook 'after-save-hook 'TeX-fold-buffer nil t)))
           (LaTeX-mode . turn-on-reftex)
           (LaTeX-mode . add-auctex-keys)
           (LaTeX-mode . LaTeX-math-mode)
           (LaTeX-mode . display-line-numbers-mode)
           ;; (TeX-mode . my-add-auctex-file-variables)
           (plain-TeX-mode . (lambda ()
                               (setq flycheck-disabled-checkers '(tex-chktex))))
           (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
           (LaTeX-mode . (lambda ()
                           (if (null (TeX-PDF-mode))
                               (command-execute 'TeX-PDF-mode))))
           (LaTeX-mode . (lambda ()
                           (local-set-key (kbd "C-c C-f") 'latex-indent))))
    :config
    ;; Set custom math environments for `texmathp'. The default list
    ;; is set in `texmathp-tex-commands-default'.
    (setq texmathp-tex-commands '(("equation*" env-on)
                                  ("align" env-on)
                                  ("align*" env-on)))
    ;; Use latex-mode by default when it is unclear whether plain-tex-mode or latex-mode should be
    ;; used.
    (setq-default TeX-auto-save t
                  TeX-parse-self t
                  TeX-master nil
                  TeX-source-correlate-start-server t
                  reftex-plug-into-AUCTeX t)
    (eval-after-load "tex-fold"
      '(add-to-list 'TeX-fold-macro-spec-list '("{2}" ("href"))))
    (eval-after-load "tex-fold"
      '(add-to-list 'TeX-fold-macro-spec-list '("{1}" ("hyperref"))))
    (eval-after-load "tex-fold"
      '(add-to-list 'TeX-fold-macro-spec-list '("{2}" ("mintinline"))))
    (eval-after-load "tex-fold"
      '(add-to-list 'TeX-fold-macro-spec-list '("{2}" ("hyperlink"))))
    ;; Disable default syntax highlighting in certain LaTeX environments.  This prevents certain
    ;; special characters from causing issues in those environments.  For instance, $ and _ See:
    ;; https://tex.stackexchange.com/questions/111289/how-to-make-auctex-ignore-syntax-highlighting-within-environment
    (setq LaTeX-verbatim-environments-local
          '("Verbatim" "lstlisting" "minted" "lstinline" "mintinline")))

  (use-package auctex-latexmk
    :after tex-site
    :demand t
    :config
    (auctex-latexmk-setup)
    (setq auctex-latexmk-inherit-TeX-PDF-mode t))

  :postsetup
  (:layer lsp
   (add-hook 'TeX-mode-hook 'lsp))

  :func
  (defun mh//simplify-tex-string ()
    (let ((str (buffer-string)))
      (shell-command-to-string (concat "python "
                                       (file-truename user-emacs-directory)
                                       "scripts/sympy_simplify_tex.py"
                                       " '"
                                       str
                                       "'"))))

  (defun mh/simplify-tex (beg end)
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list nil nil)))
    (if (and beg end)
        (replace-region-contents beg end 'mh//simplify-tex-string))))

;;; tex-layer.el ends here
