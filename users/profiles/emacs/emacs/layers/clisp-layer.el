;;; clisp-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def clisp
  :presetup
  (:layer straight
   (straight-use-package 'slime)
   (straight-use-package 'slime-company))

  :setup
  (use-package slime
    :init
    (require 'slime-autoloads)
    (add-to-list 'same-window-buffer-names "*slime-repl.*"))

  :postsetup
  (:layer nix
   (setq inferior-lisp-program "~/.nix-profile/bin/sbcl"))
  (:layer completions
   (use-package slime-company
     :after (slime company)
     :config
     (slime-setup '(slime-fancy slime-company)))))

;;; clisp-layer.el ends here
