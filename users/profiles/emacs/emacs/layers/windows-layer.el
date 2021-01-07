;;; windows-layer.el --- Window Behavior -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def windows
  :depends (base)

  :presetup
  (:layer straight
   (straight-use-package 'framemove)
   ;; (straight-use-package 'zoom)
   )

  :setup
  (use-package framemove
    :config
    (windmove-default-keybindings)
    (setq framemove-hook-into-windmove t))

  ;; tame Emacs's crazy window defaults.
  (add-to-list 'display-buffer-alist
               '("\\*compilation\\*"
                 (display-buffer-reuse-window display-buffer-same-window)))

  (defun mh/switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (error "Minibuffer is not active")))

  ;; (use-package zoom
  ;;   :config
  ;;   (custom-set-variables
  ;;    '(zoom-mode t)
  ;;    '(zoom-size '(100 . 60))
  ;;    '(zoom-ignored-major-modes '(calc-mode helm-mode))
  ;;    '(zoom-ignored-buffer-name-regexps '("^*calc"))
  ;;    '(zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))))

  ;; (setq display-buffer-alist
  ;;       '(("\\*compilation\\*"
  ;;          (display-buffer-reuse-window display-buffer-same-window))
  ;; default
  ;; (".*"
  ;;  (display-buffer-same-window))

  ;; (setq display-buffer-reuse-frames t)         ; reuse windows in other frames
  ;; (setq pop-up-windows nil)                    ; display-buffer: avoid splitting
  ;; (setq even-window-heights nil)               ; display-buffer: avoid resizing

  ;;   (defun mh//display-popup-buffer-respect-monitors (buffer alist)
  ;;     "TODO by requiring 4 windows, this function is very
  ;; limited. It should be able to find out which physical monitor
  ;; is being used and select and display the window that way.

  ;; use
  ;; (window-pixel-left)
  ;; and check if >= to 3840

  ;; Then need to determine whether to split window or if not reuse
  ;; another.

  ;; "
  ;;     (let* ((windows (window-list nil nil (frame-first-window)))
  ;;            (pos (cl-position (get-buffer-window) windows)))
  ;;       (if (string= major-mode (with-current-buffer buffer major-mode))
  ;;           (window--display-buffer buffer (nth pos windows) 'reuse alist)
  ;;         (if (equalp (length windows) 4)
  ;;             (if (equalp pos 3)
  ;;                 (window--display-buffer buffer (nth 2 windows) 'reuse alist)
  ;;               (if (equalp pos 2)
  ;;                   (window--display-buffer buffer (nth 3 windows) 'reuse alist)
  ;;                 (if (equalp pos 1)
  ;;                     (window--display-buffer buffer (nth 0 windows) 'reuse alist)
  ;;                   (if (equalp pos 0)
  ;;                       (window--display-buffer buffer (nth 1 windows) 'reuse alist)))))))))
  :postsetup
  ;; (:layer modal
  ;; (general-define-key
  ;;  :keymaps 'mh/prefix-window-map
  ;;  "h" 'evil-window-left
  ;;  "j" 'evil-window-down
  ;;  "k" 'evil-window-up
  ;;  "l" 'evil-window-right
  ;;  "H" 'evil-window-move-far-left
  ;;  "J" 'evil-window-move-very-bottom
  ;;  "K" 'evil-window-move-very-top
  ;;  "L" 'evil-window-move-far-right))

  (:layer (hydra modal)
   (defhydra window-nav-hydra (:pre (set-cursor-color "slate blue")
                               :post (set-cursor-color mh-cursor-color)
                               :timeout 1)
     "window navigation"
     ("h" evil-window-left "left")
     ("j" evil-window-down "down")
     ("k" evil-window-up "up")
     ("l" evil-window-right "right")
     ("H" evil-window-move-far-left)
     ("J" evil-window-move-very-bottom)
     ("K" evil-window-move-very-top)
     ("L" evil-window-move-far-right)
     ("d" delete-window "delete")
     ("f" delete-other-window "delete other")
     ("s" split-window-below "split vertical")
     ("S" split-window-right "split horizontal")
     ("m" mh/switch-to-minibuffer "switch to minibuffer"))

   (general-define-key
    :keymaps 'mh/prefix-map
    "w" 'window-nav-hydra/body)))

;;; windows-layer.el ends here
