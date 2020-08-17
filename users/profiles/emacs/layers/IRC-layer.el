;;; irc-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def irc
  :setup
  (use-package erc
    :hook (erc-mode . (lambda ()
                        (setq-local fill-column nil)))
    :config
    (setq erc-prompt-for-password nil)
    (setq erc-nick "matthuszagh")
    ;; allow notifications
    (add-to-list 'erc-modules 'notifications)
    (defun mh/erc-freenode-connect ()
      (interactive)
      (erc :server "irc.freenode.net" :port 6667 :nick "matthuszagh"))

    (defun mh/erc-bitlebee-connect ()
      (interactive)
      (erc :server "localhost" :port 6667 :nick "matthuszagh")))

  :postsetup
  (:layer modal
   (general-def mh/prefix-help-map
     "e" 'mh/erc-freenode-connect
     "E" 'mh/erc-bitlebee-connect)))

;;; irc-layer.el ends here
