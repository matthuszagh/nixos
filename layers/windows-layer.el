;;; windows-layer.el --- Window Behavior -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def windows
  :setup
  (defun mh//display-popup-buffer-respect-monitors (buffer alist)
    (let* ((windows (window-list nil nil (frame-first-window)))
           (pos (cl-position (get-buffer-window) windows)))
      (if (string= major-mode (with-current-buffer buffer major-mode))
          (window--display-buffer buffer (nth pos windows) 'reuse alist)
        (if (equalp (length windows) 4)
            (if (equalp pos 3)
                (window--display-buffer buffer (nth 2 windows) 'reuse alist)
              (if (equalp pos 2)
                  (window--display-buffer buffer (nth 3 windows) 'reuse alist)
                (if (equalp pos 1)
                    (window--display-buffer buffer (nth 0 windows) 'reuse alist)
                  (if (equalp pos 0)
                      (window--display-buffer buffer (nth 1 windows) 'reuse alist))))))))))
  :postsetup
  (:layer modal
   (general-define-key
    :keymaps 'mh/prefix-window-map
    "h" 'evil-window-left
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right
    "H" 'evil-window-move-far-left
    "J" 'evil-window-move-very-bottom
    "K" 'evil-window-move-very-top
    "L" 'evil-window-move-far-right))

  (:layer (hydra modal)
   (defhydra window-nav-hydra ()
     "window navigation"
     ("h" evil-window-left)
     ("j" evil-window-down)
     ("k" evil-window-up)
     ("l" evil-window-right)
     ("H" evil-window-move-far-left)
     ("J" evil-window-move-very-bottom)
     ("K" evil-window-move-very-top)
     ("L" evil-window-move-far-right))
   (general-define-key
    :keymaps 'mh/prefix-window-map
    "n" 'window-nav-hydra/body)))

;;; windows-layer.el ends here
