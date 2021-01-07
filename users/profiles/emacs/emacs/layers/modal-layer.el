;;; modal-layer.el --- Modal Editing Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def modal
  :presetup
  (:layer straight
   (setq evil-want-keybinding nil)
   (straight-use-package 'evil)
   (straight-use-package 'evil-collection)
   (straight-use-package 'evil-surround)
   (straight-use-package 'evil-numbers))

  (:layer keybinding-management
   ;; Commands begin with `SPC' in normal mode and `C-SPC' in insert mode.
   (general-define-key
    :states '(emacs normal insert visual)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    :prefix-command 'mh/command-prefix
    :prefix-map 'mh/prefix-map)

   ;; major mode bindings
   (general-create-definer localleader
     :states '(emacs normal insert visual)
     :prefix ","
     :non-normal-prefix "C-,")

   ;; bindings for programming modes, but agnostic to the
   ;; specific language. e.g. jump to definition
   (general-define-key
    :states 'normal
    "g d" 'xref-find-definitions
    "g p" 'xref-pop-marker-stack
    "g l" 'goto-line)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "p"
    :prefix-command 'mh/command-prog-prefix
    :prefix-map 'mh/prefix-prog-map)

   ;; (general-define-key
   ;;  :keymaps 'mh/prefix-map
   ;;  :prefix "w"
   ;;  :prefix-command 'mh/command-window-prefix
   ;;  :prefix-map 'mh/prefix-window-map
   ;;  "d" 'delete-window
   ;;  "f" 'delete-other-windows
   ;;  "S" 'split-window-below
   ;;  "s" 'split-window-right
   ;;  "m" 'mh/switch-to-minibuffer)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "b"
    :prefix-command 'mh/command-buffer-prefix
    :prefix-map 'mh/prefix-buffer-map
    "r" 'revert-buffer
    "k" 'kill-buffer
    "c" 'clone-indirect-buffer-other-window)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "f"
    :prefix-command 'mh/command-file-prefix
    :prefix-map 'mh/prefix-file-map
    "s" 'basic-save-buffer
    "o" 'find-file-other-window
    ;; "f" 'find-file
    "F" 'mh/sudo-find-file
    "c" 'mh/copy-file-name
    "C" 'mh/copy-file-path)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "a"
    :prefix-command 'mh/command-appearance-prefix
    :prefix-map 'mh/prefix-appearance-map
    "=" 'mh/zoom-in-selected-frame
    "-" 'mh/zoom-out-selected-frame
    "+" 'mh/zoom-in
    "_" 'mh/zoom-out)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "z"
    :prefix-command 'mh/command-format-prefix
    :prefix-map 'mh/prefix-format-map
    "i" 'mh/indent-buffer
    "u" 'ucs-insert)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "s"
    :prefix-command 'mh/command-search-prefix
    :prefix-map 'mh/prefix-search-map
    "r" 'query-replace-regexp)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "c"
    :prefix-command 'mh/command-shell-prefix
    :prefix-map 'mh/prefix-shell-map)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "h"
    :prefix-command 'mh/command-help-prefix
    :prefix-map 'mh/prefix-help-map)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "n"
    :prefix-command 'mh/command-calc-prefix
    :prefix-map 'mh/prefix-calc-map)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "u"
    :prefix-command 'mh/command-undo-prefix
    :prefix-map 'mh/prefix-undo-map)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "x"
    :prefix-command 'mh/command-system-prefix
    :prefix-map 'mh/prefix-system-map)

   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "m"
    :prefix-command 'mh/command-mark-prefix
    :prefix-map 'mh/prefix-mark-map
    "a" 'mark-whole-buffer)

   ;; minibuffer keybindings
   (general-define-key
    :keymaps 'minibuffer-local-map
    "C-j" 'next-history-element
    "C-k" 'previous-history-element))

  :setup
  (setq evil-want-keybinding nil)

  (use-package evil
    :init
    ;; this prevents annoying abbrev expands in verilog mode.
    (setq evil-want-abbrev-expand-on-insert-exit nil)
    (setq evil-want-integration t) ; This is optional since it's already set to t by default.
    :config
    (evil-mode 1))

  (use-package evil-collection
    :after (evil)
    :init
    :config
    (evil-collection-init)

    ;; TODO relocate these to appropriate postinits.
    ;; TODO use general-def instead of evil-define-key
    ;; Jump to the bottom of the window when entering insert mode in terminal.
    ;; comint-mode configuration
    ;;
    ;; Same behavior for comint modes. Prevent this when in the middle of the line at the command
    ;; line. This allows evil navigation to edit the current command. I'd like this for term-mode too,
    ;; but it's much trickier (see emacs tex file).
    (evil-collection-define-key 'normal 'comint-mode-map (kbd "i")
      (lambda ()
        (interactive)
        (if (eq (line-number-at-pos)
                (+ (evil-count-lines (point-min) (point-max)) 1))
            (evil-insert-state)
          (progn (comint-show-maximum-output)
                 (evil-insert-state)))))
    ;; Use C-p and C-n to cycle through inputs for consistency with term-mode.
    (evil-collection-define-key 'insert 'comint-mode-map
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)

    ;; doc-view-mode configuration
    ;;
    ;; Use the same page navigation in doc-view as in pdf-mode.
    (evil-collection-define-key 'normal 'doc-view-mode-map (kbd "j") 'doc-view-next-page)
    (evil-collection-define-key 'normal 'doc-view-mode-map (kbd "k") 'doc-view-previous-page)

    ;; org-mode configuration
    ;; (evil-collection-define-key 'normal 'org-mode-map (kbd "<tab>") 'org-cycle)

    ;; proced-mode configuration
    (evil-collection-define-key 'normal 'proced-mode-map (kbd "q") (lambda () (interactive)
                                                                     (quit-window)
                                                                     (command-execute 'symon-mode))))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  (use-package evil-numbers
    :general
    (:states '(normal visual emacs)
     "=" 'evil-numbers/inc-at-pt
     "-" 'evil-numbers/dec-at-pt)))

;;; modal-layer.el ends here
