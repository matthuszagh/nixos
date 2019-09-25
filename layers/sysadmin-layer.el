;;; sysadmin-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def sysadmin
  :setup
  (use-package proced
    :config
    (add-to-list 'same-window-buffer-names "*Proced*"))

  ;; Display CPU/mem/etc usage.
  (use-package symon
    :init
    ;; No delay when updating usage.
    (setq symon-delay 0)
    (setq symon-refresh-rate 1))

  ;; query available packages for emacs
  (use-package paradox
    :config
    (paradox-enable)
    (setq paradox-github-token nil))

  :postsetup
  (:layer modal
          (general-def mh/prefix-system-map
            "p" 'proced
            "s" 'symon-mode
            "c" 'display-time-mode
            "b" 'display-battery-mode))

  (:layer helm
          (use-package helm-systemd))

  (:layer (helm modal)
          (general-def mh/prefix-system-map
            "d" 'helm-systemd)))

;;; sysadmin-layer.el ends here
