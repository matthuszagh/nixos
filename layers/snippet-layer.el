;;; snippet-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def snippet
  :setup
  (use-package yasnippet
    :config
    (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets/"))
    (yas-reload-all))

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
   (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)))

;;; snippet-layer.el ends here
