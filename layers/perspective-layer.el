;;; perspective-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def perspective
  :setup
  (use-package perspective
    :config
    (persp-mode)
    (setq persp-state-default-file (concat user-emacs-directory "var/perspective/save"))
    (add-hook 'kill-emacs-hook #'persp-state-save))

  :postsetup
  (:layer modal
   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "d"
    :prefix-command 'mh/command-desktop-prefix
    :prefix-map 'mh/prefix-desktop-map)
   (general-define-key
    :keymaps 'mh/prefix-desktop-map
    "s" 'persp-switch
    "b" 'persp-set-buffer
    "k" 'persp-kill)))

;;; perspective-layer.el ends here
