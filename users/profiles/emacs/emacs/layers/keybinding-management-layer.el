;;; keybinding-management-layer.el --- Keybinding layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def keybinding-management
  :depends (base)
  :presetup
  (:layer straight
   (straight-use-package 'general)
   (straight-use-package 'which-key))

  ;; (:layer (straight helm)
  ;;  (straight-use-package 'helm-descbinds))

  :setup
  (use-package general
    :config
    (add-to-list 'same-window-buffer-names "*General Keybindings*"))

  (use-package which-key
    :functions which-key-mode
    :config
    (which-key-mode))

  :postsetup
  (:layer helm
   ;; TODO shouldn't be here
   (straight-use-package 'helm-descbinds)
   (use-package helm-descbinds
     :after helm
     :config
     (helm-descbinds-mode)))

  (:layer (helm modal)
   (general-define-key
    :keymaps 'mh/prefix-help-map
    "b" 'helm-descbinds)))

;;; keybinding-management-layer.el ends here
