;;; base-layer.el --- Base Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def base
  :setup
  ;; Increases the garbage collection threshold. Specifically, this
  ;; delays garbage collection until Emacs has allocated 100MB from
  ;; memory. Setting this value too low triggers garbage collection
  ;; frequently and adversely affects performance. Setting it too high
  ;; can deplete your available memory and slow down the entire
  ;; system.
  (setq gc-cons-threshold 100000000)

  ;; LSP (and maybe other packages) need to read large amounts of data
  ;; at a time from subprocesses. The default setting is overly
  ;; restrictive.
  (setq read-process-output-max (* 10 1024 1024)) ;; 10MB

  ;; Always start Emacs maximized.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Only show cursor in active window.
  (setq-default cursor-in-non-selected-windows nil)
  ;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

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
  (setq split-height-threshold 100)

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

  ;;; Delete trailing whitespace when saving.
  ;; Configure this as a customizable variable. This allows disabling
  ;; trailing whitespace deletion when the variable is set to
  ;; nil. This is useful when working on someone else's project which
  ;; has trailing whitespace.
  (defvar mh-delete-trailing-whitespace t)
  (add-hook 'before-save-hook (lambda ()
                                (if mh-delete-trailing-whitespace
                                    (delete-trailing-whitespace))))

  ;; enable y/n answers
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; ignore undo discard info warnings
  (setq warning-suppress-types '((undo discard-info)))

  ;; TODO this makes many rust files executable which shouldn't be.
  ;; ;; automatically make relevant files executable
  ;; (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  ;; Emacs modes typically provide a standard means to change the
  ;; indentation width -- eg. c-basic-offset: use that to adjust your
  ;; personal indentation width, while maintaining the style (and
  ;; meaning) of any files you load.
  (setq-default indent-tabs-mode nil) ; don't use tabs to indent
  (setq-default tab-width 8)          ; but maintain correct appearance

  ;; Set default font.
  ;; TODO set font based on screen dimensions.
  (defconst mh-font "Source Code Pro")
  (defconst mh-font-size 8)
  (add-to-list 'default-frame-alist
               `(font . ,(concat mh-font "-" (number-to-string mh-font-size))))

  ;; This permits folding throughout Emacs. For instance, Evil's fold capabilities rely on this being
  ;; set.
  (setq outline-minor-mode t)

  ;; don't truncate message log buffer
  (setq message-log-max t)

  ;; Newline at end of file.
  (setq require-final-newline t)

  ;; Don't insert pairs in the minibuffer.
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (electric-pair-local-mode 'toggle)))

  ;; Never ring the bell.
  (setq ring-bell-function 'ignore)

  ;; ;; Automatically save buffers visiting files, so that I don't have
  ;; ;; to do it manually.
  ;; (auto-save-visited-mode)

  ;; Disable performance-affecting features when lines become very long
  (global-so-long-mode 1)

  ;; TODO these should probably be in func and customize.
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

  (setq mh-face-attribute-height 80)
  (defun mh/zoom-in ()
    (interactive)
    (setq mh-face-attribute-height (+ mh-face-attribute-height 10))
    (set-face-attribute 'default nil :height mh-face-attribute-height))

  (defun mh/zoom-in-selected-frame ()
    (interactive)
    (set-face-attribute 'default (selected-frame) :height
                        (+ (face-attribute 'default :height) 10)))

  (defun mh/zoom-out ()
    (interactive)
    (setq mh-face-attribute-height (- mh-face-attribute-height 10))
    (set-face-attribute 'default nil :height
                        mh-face-attribute-height))

  (defun mh/zoom-out-selected-frame ()
    (interactive)
    (set-face-attribute 'default (selected-frame) :height
                        (- (face-attribute 'default :height) 10)))

  :func
  (defun mh/insert-rand-password-at-point (len include-sp-chars)
    (interactive "nlength: \nMInclude special characters? (y/n): ")
    ;; head /dev/urandom | tr -dc A-Za-z0-9 | head -c 13 ; echo ''
    ;; </dev/urandom tr -dc 'A-Za-z0-9!"#$%&'\''()*+,-./:;<=>?@[\]^_`{|}~' | head -c 13  ; echo
    (let ((output ""))
      (if (string-equal include-sp-chars "y")
          (setq output (shell-command-to-string
                        (concat "</dev/urandom tr -dc "
                                "'A-Za-z0-9!\"#$%&'\\''()*+,-./:;<=>?@[\\]^_`{|}~'"
                                " | head -c "
                                (number-to-string len)
                                " ; echo")))
        (setq output (shell-command-to-string
                      (concat "head /dev/urandom | tr -dc A-Za-z0-9 "
                              "| head -c "
                              (number-to-string len)
                              " ; echo ''"))))
      (insert output)))

  (defun mh/clear-image-cache ()
    "Sometimes Emacs shows the wrong image when it thinks an image
hasn't changed. This clears the image cache to prevent this."
    (interactive)
    (clear-image-cache t))

  (defun mh/insert-current-date ()
    (interactive)
    (insert (shell-command-to-string "echo -n $(date +%d/%m/%Y)")))

  (defun mh/time-stamp ()
    (format-time-string "[%Y-%m-%d %a %H:%M]"))

  (defun mh/screenshot-svg ()
    "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
    (interactive)
    (let* ((filename (make-temp-file "Emacs" nil ".svg"))
           (data (x-export-frames nil 'svg)))
      (with-temp-file filename
        (insert data))
      (kill-new filename)
      (message filename)))

  (defun mh//inc-char (char)
    "Increment CHAR.
For instance this will perform 'a' -> 'b'"
    (string (1+ (string-to-char char))))

  (defun mh/point-at-line-begp ()
    "Indicate whether point is at the beginning of a line."
    (let ((beg-pos (line-beginning-position)))
      (eq (point) beg-pos)))

  (defun mh/replace-all-alist-items-in-current-buffer (alist)
    "Take an association list ALIST and replace the first item in each alist pair with the second item."
    (dolist (elt alist)
      (goto-char 0)
      (while (re-search-forward (car elt) nil t)
        (replace-match (cdr elt) t)))))

;;; base-layer.el ends here
