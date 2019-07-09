;;; exwm-layer.el -*-lexical-binding: t; -*-

;;; Code:

(layer-def exwm
  :presetup
  (:layer straight
          (straight-use-package 'exwm))

  :setup
  (use-package exwm
    :config
    (require 'exwm-config)
    (exwm-enable)
    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist '(0 "eDP-1-1" 1 "DP-0" 2 "DP-2"))
    (add-hook 'exwm-randr-screen-change-hook
              (lambda ()
                (start-process-shell-command
                 "xrandr" nil (concat "xrandr --output eDP-1-1 --auto "
                                      "--output DP-0 --left-of eDP-1-1 "
                                      "--output DP-2 --right-of eDP-1-1"))))
    (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1-1" 1 "DP-0" 2 "DP-2"))
    (exwm-randr-enable))

  :postsetup
  (:layer modal-interaction
          ;; stop exwm from catching SPC leader key.
          ;; the double whitespace is intentional
          (add-to-list 'exwm-input-prefix-keys ?\C-\  )
          (evil-set-initial-state 'exwm-mode 'insert)))

;;; exwm-layer.el ends here
