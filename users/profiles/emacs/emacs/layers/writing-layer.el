;;; writing-layer.el --- Writing Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def writing
  :presetup
  (:layer straight
   (straight-use-package 'writegood-mode)
   (straight-use-package '(langtool :host github :repo "redguardtoo/Emacs-langtool"))
   (straight-use-package 'define-word))

  :setup
  (use-package writegood-mode
    :functions writegood-mode)

  (use-package langtool
    :config
    (setq langtool-bin "languagetool-commandline"))

  :postsetup
  (:layer org
   (add-hook 'org-mode 'writegood-turn-on))
  (:layer vcs
   (add-hook 'git-commit-setup 'writegood-mode))
  (:layer modal
   (general-define-key
    :keymaps 'mh/prefix-help-map
     "w" 'define-word-at-point
     "W" 'define-word)))

;;; writing-layer.el ends here
