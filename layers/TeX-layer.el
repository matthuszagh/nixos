;;; tex-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def tex
  :presetup
  (:layer straight
   (straight-use-package 'auctex))

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

    ;; shortcuts for using auctex
    (defun insert-frac ()
      "Insert '\frac{}{}' and position point before the first right brace."
      (interactive)
      (progn
        (insert "\\frac{}{}")
        (backward-char)
        (backward-char)
        (backward-char)))

    (defun insert-text ()
      "Insert '\text{}' and position point inside the brace."
      (interactive)
      (progn
        (insert "\\text{}")
        (backward-char)))

    (defun insert-math-subscript ()
      "Insert '_{\text{}}' and cursor to point inside middle brace."
      (interactive)
      (progn
        (insert "_{\\text{}}")
        (backward-char)
        (backward-char)))

    (defun insert-left-delimiter ()
      "Insert '\left('."
      (interactive)
      (progn
        (insert "\\left(")))

    (defun insert-right-delimiter ()
      "Insert '\right)'."
      (interactive)
      (progn
        (insert "\\right)")))

    (defun add-auctex-keys ()
      ;; Make C-i distinguishable from tab.
      (define-key input-decode-map "\C-i" [C-i])
      (local-set-key (kbd "C-c <C-i> f") 'insert-frac)
      (local-set-key (kbd "C-c <C-i> t") 'insert-text)
      (local-set-key (kbd "C-c <C-i> s") 'insert-math-subscript)
      (local-set-key (kbd "C-c <C-i> l") 'insert-left-delimiter)
      (local-set-key (kbd "C-c <C-i> r") 'insert-right-delimiter)
      (local-set-key (kbd "C-c C-f") 'indent-buffer))

    (defun latex-indent ()
      "Run latexindent on the current buffer."
      (interactive)
      (with-no-warnings
        (shell-command (concat "latexindent " (buffer-file-name) " > " (buffer-file-name) ".tmp && mv "
                               (buffer-file-name) ".tmp " (buffer-file-name)))))

    ;; When we byte-compile we need to have the autoloads loaded in order to
    ;; properly get auctex working, otherwise auctex is not loaded correctly
    (load "auctex-autoloads" nil t)
    :hook ((LaTeX-mode . TeX-source-correlate-mode)
           (LaTeX-mode . auto-fill-mode)
           (LaTeX-mode . flyspell-mode)
           (LaTeX-mode . flyspell-buffer)
           (LaTeX-mode . (lambda ()
                           (TeX-fold-mode 1)
                           (add-hook 'find-file-hook 'TeX-fold-buffer t t)
                           (add-hook 'after-save-hook 'TeX-fold-buffer nil t)))
           (LaTeX-mode . turn-on-reftex)
           (LaTeX-mode . add-auctex-keys)
           (LaTeX-mode . LaTeX-math-mode)
           (TeX-mode . my-add-auctex-file-variables)
           (plain-TeX-mode . (lambda ()
                               (setq flycheck-disabled-checkers '(tex-chktex))))
           (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
           (LaTeX-mode . (lambda ()
                           (if (null (TeX-PDF-mode))
                               (command-execute 'TeX-PDF-mode))))
           (LaTeX-mode . (lambda ()
                           (local-set-key (kbd "C-c C-f") 'latex-indent))))
    :config
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
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)))

;;; tex-layer.el ends here
