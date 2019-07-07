;;; irc-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def irc
  :setup
  (use-package erc
    :hook (erc-mode . (lambda ()
                        (setq-local fill-column nil)))
    :config
    (setq erc-prompt-for-password nil)))

;;; irc-layer.el ends here
