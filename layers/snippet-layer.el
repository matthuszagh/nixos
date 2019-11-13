;;; snippet-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def snippet
  :setup
  (use-package yasnippet
    :config
    (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets/")))

  (use-package yasnippet-snippets
    :config
    (yas-reload-all))

  :postsetup
  (:layer TeX
   (add-hook 'latex-mode-hook #'yas-minor-mode))

  (:layer org
   (add-hook 'org-mode-hook #'yas-minor-mode)))

;;; snippet-layer.el ends here
