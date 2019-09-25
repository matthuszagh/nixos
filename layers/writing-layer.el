;;; writing-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def writing
  :presetup
  (:layer straight
   (straight-use-package 'writegood-mode))
  :setup
  (use-package writegood-mode
    :functions writegood-mode)

  :postsetup
  (:layer org
   (add-hook 'org-mode 'writegood-mode))
  (:layer vcs
   (add-hook 'git-commit-setup 'writegood-mode)))

;;; writing-layer.el ends here
