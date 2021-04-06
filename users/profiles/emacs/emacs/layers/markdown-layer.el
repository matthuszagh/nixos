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
    :init (setq markdown-command "multimarkdown"))

  :postsetup
  (:layer modal
   (general-define-key
    :keymaps 'markdown-mode-map
    :states '(normal motion)
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "$" 'evil-end-of-line-or-visual-line
    "0" 'evil-beginning-of-visual-line
    "V" 'evil-visual-screen-line)))

;;; markdown-layer.el ends here
