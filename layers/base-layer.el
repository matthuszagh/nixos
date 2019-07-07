;;; base-layer.el --- Base Layer -*-lexical-binding: t; -*-

;;; Code:

(layer-def base
  :setup
  ;; Increases the garbage collection threshold. Specifically, this delays garbage collection until
  ;; Emacs has allocated 100MB from memory. Setting this value too low triggers garbage collection
  ;; frequently and adversely affects performance. Setting it too high can deplete your available
  ;; memory and slow down the entire system.
  (setq gc-cons-threshold 100000000)

  ;; Prefer `.el' files over outdated `.elc' files. Use this with `auto-compile' to automatically
  ;; byte-compile outdated files.
  (setq load-prefer-newer t)

  ;; Always start Emacs maximized.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Disable toolbar.
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; Disable menu-bar.
  (menu-bar-mode -1)

  ;; Disable cursor blinking.
  (blink-cursor-mode -1)

  ;; Draw the block cursor as wide as the glyph under it. This mostly helps dileneate spaces from
  ;; tabs.
  (setq x-stretch-cursor t)

  ;; Increase splitting threshold so that new buffers don't split existing ones.
  ;; TODO improve documentation of this setting.
  (setq split-height-threshold 160)

  ;; Disable startup screen.
  (setq inhibit-startup-screen t)

  ;; Start scrolling when the cursor is within 5 lines of the buffer limit.
  (setq scroll-margin 5)
  ;; When scrolling begins due to `scroll-margin', only scroll 1 line.
  (setq scroll-conservatively 1)
  ;; Don't keep cursor position unchanged in buffer.
  (setq scroll-preserve-screen-position nil)

  ;; Hide the scroll bar.
  (scroll-bar-mode -1)
  ;; Ensure that scrollbar is also disabled for new frames.
  (defun my/disable-scroll-bars (frame)
    (modify-frame-parameters frame
                             '((vertical-scroll-bars . nil)
                               (horizontal-scroll-bars . nil))))
  (add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

  ;; Delete trailing whitespace when saving.
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; enable y/n answers
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Emacs modes typically provide a standard means to change the
  ;; indentation width -- eg. c-basic-offset: use that to adjust your
  ;; personal indentation width, while maintaining the style (and
  ;; meaning) of any files you load.
  (setq-default indent-tabs-mode nil) ; don't use tabs to indent
  (setq-default tab-width 8)          ; but maintain correct appearance

  ;; Set default font.
  ;; TODO set font based on screen dimensions.
  (defconst mh-font "Source Code Pro")
  (defconst mh-font-size 10)
  (add-to-list 'default-frame-alist
               `(font . ,(concat mh-font "-" (number-to-string mh-font-size))))

  ;; This permits folding throughout Emacs. For instance, Evil's fold capabilities rely on this being
  ;; set.
  (setq outline-minor-mode t)

  ;; Newline at end of file.
  (setq require-final-newline t)

  ;; Auto-wrap at 70 characters. This is disabled in programming modes.
  (setq-default auto-fill-function 'do-auto-fill)
  (setq-default fill-column 70)
  (turn-on-auto-fill)

  ;; Never ring the bell.
  (setq ring-bell-function 'ignore)

  ;; Disable auto-save.
  ;; (setq auto-save-default nil)

  (defun mh/sudo-find-file (file-name)
    "Like find-file, but opens FILE-NAME as root."
    (interactive "FSudo Find File: ")
    (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
      (find-file tramp-file-name)))

  (defun mh/copy-untabify (start end)
    "Copy the region from START to END.
  Tabs at the beginning of each line are replaced by the equivalent
  amount of spaces. This is often useful when pasting outside
  Emacs."
    (interactive "r")
    (kill-new
     (replace-regexp-in-string
      "^\t+"
      (lambda (substring)
        (make-string (* tab-width (length substring)) ?\s))
      (buffer-substring start end))))

  (defun mh/copy-file-path ()
    "Copy the absolute file path of the current file to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (with-temp-buffer
          (insert filename)
          (clipboard-kill-region (point-min) (point-max)))
        (message filename))))

  (defun mh/copy-file-name ()
    "Copy the file name of the current file to the clipboard."
    (interactive)
    (let ((buffername (buffer-name)))
      (when buffername
        (with-temp-buffer
          (insert buffername)
          (clipboard-kill-region (point-min) (point-max)))
        (message buffername))))

  (defun mh/indent-buffer ()
    "Indent the entire buffer."
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max) nil)))

  (defun mh/switch-to-minibuffer ()
    "Switch to minibuffer window."
    (interactive)
    (if (active-minibuffer-window)
        (select-window (active-minibuffer-window))
      (error "Minibuffer is not active"))))

;;; base-layer.el ends here
