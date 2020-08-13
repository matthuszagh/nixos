(in-package :next)

;; use vi keybindings by default
(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; zoom for HiDPI display
(defun mh-buffer-defaults (buffer)
  (setf (zoom-ratio-default buffer) 1.7)
  (unzoom-page :buffer buffer))          ; Needed for non-web-mode buffers.

(defun mh-minibuffer-defaults (minibuffer)
  (setf
   (max-lines minibuffer) 12
   (minibuffer-line-height minibuffer) "1.1em"
   (minibuffer-font-size minibuffer) "1.1em"))

(defun mh-minibuffer-window-defaults (window)
  (setf
   (minibuffer-open-height window) 400))

(defun mh-interface-defaults ()
  (hooks:add-to-hook (hooks:object-hook *interface* 'buffer-make-hook)
                     #'mh-buffer-defaults)
  (hooks:add-to-hook (hooks:object-hook *interface* 'minibuffer-make-hook)
                     #'mh-minibuffer-defaults)
  (hooks:add-to-hook (hooks:object-hook *interface* 'window-make-hook)
                     #'mh-minibuffer-window-defaults))

(hooks:add-to-hook '*after-init-hook* #'mh-interface-defaults)
