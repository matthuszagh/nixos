;;; style-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def help
  :presetup
  (:layer straight
          (straight-use-package 'writegood-mode))
  :setup
  (use-package writegood-mode
    :functions writegood-mode
    :hook ((org-mode . writegood-mode)
           (git-commit-setup . writegood-mode))))

;;; style-layer.el ends here
