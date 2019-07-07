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
                 "xrandr1" nil "xrandr --output eDP-1-1 --right-of DP-0 --auto")))
    ;; "xrandr1" nil "xrandr --output DP-0 --left-of eDP-1-1 --auto")))
    ;; "xrandr1" nil "xrandr --output DP-0 --left-of eDP-1-1 --auto")))
    ;;              (start-process-shell-command
    ;;               "xrandr2" nil "xrandr --output DP-2 --right-of eDP-1-1 --auto")))
    (exwm-randr-enable)))

;;; exwm-layer.el ends here
