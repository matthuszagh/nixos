;;; exwm-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def exwm
  :presetup
  (:layer straight
   (straight-use-package 'exwm)
   (straight-use-package 'pulseaudio-control))

  :setup
  (use-package exwm
    :init
    (setq exwm-workspace-number 3)
    ;; s-N switches to workspace N and s-w interactively switches.
    (setq mh--exwm-window-pixel-delta 100)
    (defun mh/exwm-enlarge ()
      (interactive)
      (call-interactively 'exwm-layout-enlarge-window t (vector mh--exwm-window-pixel-delta))
      (call-interactively 'exwm-layout-enlarge-window-horizontally t (vector mh--exwm-window-pixel-delta)))

    (defun mh/exwm-shrink ()
      (interactive)
      (call-interactively 'exwm-layout-enlarge-window t (vector (math-neg mh--exwm-window-pixel-delta)))
      (call-interactively 'exwm-layout-enlarge-window-horizontally t (math-neg mh--exwm-window-pixel-delta)))
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset)
            ([?\s-i] . exwm-input-release-keyboard)
            ;; convenience keybinding, meant to mimic `esc' in Vim bindings
            ([?\s-\[] . exwm-reset)
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-f] . exwm-floating-toggle-floating)
            ;; move floating window with s+vim-like keys
            ([?\s-j] . (lambda ()
                         (exwm-floating-move 0 -1)))
            ([?\s-k] . (lambda ()
                         (exwm-floating-move 0 1)))
            ([?\s-h] . (lambda ()
                         (exwm-floating-move -1 0)))
            ([?\s-l] . (lambda ()
                         (exwm-floating-move 1 0)))
            ([?\s-\=] . exwm-layout-enlarge-window)
            ([?\s-\+] . exwm-layout-enlarge-window)
            ([?\s-\-] . exwm-layout-shrink-window)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))
            ;; launch apps with s-&
            ([?\s-&] . (lambda (command)
		         (interactive (list (read-shell-command "$ ")))
		         (start-process-shell-command command nil command)))))
    ;; Simulation keys allow you to send one key to EXWM and have EXWM
    ;; send another key to the application. They are only active in
    ;; line mode. This allows you to, e.g., use Vim normal mode-like
    ;; navigation keybindings globally.
    (setq exwm-input-simulation-keys
          `(
            ;; motion
            (,(kbd "h") . [left])
            ([j] . [down])
            ([?\C-j] . [C-down])
            ([k] . [up])
            ([?\C-k] . [C-up])
            ([l] . [right])
            ;; cut/copy/paste
            ([y] . [?\C-c])
            ([p] . [?\C-v])))

    :config
    (require 'exwm-config)
    (exwm-enable)
    ;; Don't send C-g to window in line mode.
    (define-key exwm-mode-map (kbd "C-g") 'keyboard-quit)

    (require 'exwm-randr)
    (exwm-randr-enable)
    ;; set monitor layout
    (if (string= "oryp4\n" (shell-command-to-string "hostname"))
        (progn
          (if (not (string-empty-p (shell-command-to-string "xrandr | grep \"DP-0 connected\"")))
              (start-process-shell-command
               "xrandr" nil (concat "xrandr --output eDP-1-1 --auto"
                                    " --output DP-0 --above eDP-1-1"
                                    " --output DP-2 --right-of DP-0"
                                    " && xrandr --setmonitor external auto DP-0,DP-2")))
          ;; "--set \"PRIME Synchronization\" 1"))))
          (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1-1" 1 "DP-0")))
      (if (string= "mbp\n" (shell-command-to-string "hostname"))
          (progn
            (if (not (string-empty-p (shell-command-to-string "xrandr | grep \"HDMI2 connected\"")))
                (start-process-shell-command
                 "xrandr" nil (concat "xrandr --output eDP1 --auto"
                                      " --output HDMI2 --above eDP1 --mode 3840x2160")))
            (setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "HDMI2")))
        (if (string= "ryzen3950\n" (shell-command-to-string "hostname"))
            (progn
              (start-process-shell-command
               "xrandr" nil (concat "xrandr --output DisplayPort-0 --auto"
                                    " --output DisplayPort-1 --right-of DisplayPort-0"
                                    " && xrandr --setmonitor main auto DisplayPort-0,DisplayPort-1"))
              (setq exwm-randr-workspace-monitor-plist '(0 "DisplayPort-0"))))))

    ;; Increase screen scaling for main computer monitor
    (if (not (string= "ryzen3950\n" (shell-command-to-string "hostname")))
        (add-hook 'exwm-init-hook
                  (lambda ()
                    (exwm-workspace-switch 0)
                    (mh/zoom-in-selected-frame)
                    (mh/zoom-in-selected-frame)))))

  (use-package pulseaudio-control)

  ;; Helps prevent screen artifacts like vertical lines left by the cursor.
  ;; (modify-all-frames-parameters '((inhibit-double-buffering . t)))

  :postsetup
  (:layer modal
   ;; stop exwm from catching SPC leader key.
   ;; the double whitespace is intentional
   (add-to-list 'exwm-input-prefix-keys ?\C-\  )
   (add-to-list 'exwm-input-prefix-keys ?\  )
   (exwm-input-set-key (kbd "s-<f5>") #'pulseaudio-control-decrease-volume)
   (exwm-input-set-key (kbd "s-<f6>") #'pulseaudio-control-increase-volume)
   (evil-set-initial-state 'exwm-mode 'normal))

  :func
  (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))
  (defun mh/launch-next ()
    (interactive)
    (let ((cmd (concat "export WEBKIT_DISABLE_COMPOSITING_MODE=1"
                       " && export $(dbus-launch)"
                       " && next")))
      (start-process-shell-command cmd nil cmd)))

  (defun mh/display-hdmi ()
    (interactive)
    ;; if audio doesn't work try xrandr --output HDMI-0 --set audio on
    (start-process-shell-command "xrandr-hdmi" nil "xrandr --output HDMI-0"))

  :customize
  (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook 'exwm-rename-buffer))

;;; exwm-layer.el ends here
