;;; markdown-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def markdown
  :presetup
  (:layer straight
   (straight-use-package 'markdown-mode))

  :setup
  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown")))

;;; markdown-layer.el ends here
