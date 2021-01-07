;;; sysadmin-layer.el --- System Administration Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def sysadmin
  :presetup
  (:layer straight
   ;; TODO fix
   ;; (straight-use-package 'symon)
   (straight-use-package 'paradox)
   (straight-use-package 'helm-systemd))

  :setup
  (use-package proced
    :config
    (add-to-list 'same-window-buffer-names "*Proced*")
    (defun mh/strace-pid-proced (expr)
      (interactive
       (list
        (read-string "trace expression: " "write")))
      (let ((pid (number-to-string (proced-pid-at-point))))
        (start-process-shell-command
         (concat "strace " pid)
         (concat "strace " pid)
         (if (string-empty-p expr)
             (concat "strace -p" pid " -s9999")
           (concat "strace -p" pid " -s9999 -e " expr))))))

  ;; TODO fix
  ;; ;; Display CPU/mem/etc usage.
  ;; (use-package symon
  ;;   :init
  ;;   ;; No delay when updating usage.
  ;;   (setq symon-delay 0)
  ;;   (setq symon-refresh-rate 1))

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
     "b" 'display-battery-mode)
   (general-define-key
    :states 'normal
    :keymaps 'proced-mode-map
    "g" 'revert-buffer
    "t" 'mh/strace-pid-proced))

  (:layer helm
   (use-package helm-systemd))

  (:layer (helm modal)
   (general-def mh/prefix-system-map
     "d" 'helm-systemd
     "l" 'helm-locate-library))

  :func
  (defun mh/start-vpn ()
    (interactive)
    (start-process-shell-command
     "pia" nil "sudo systemctl start openvpn-us-east")))

;;; sysadmin-layer.el ends here
