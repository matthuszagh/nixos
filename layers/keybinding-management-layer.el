;;; keybinding-management-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def keybinding-management
  :depends (base)
  :presetup
  (:layer straight
   (straight-use-package 'general)
   (straight-use-package 'which-key)
   ;; TODO this should depend on straight and helm
   (straight-use-package 'helm-descbinds))

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
   (use-package helm-descbinds
     :after helm
     ;; TODO this should also depend on modal
     :general
     (:keymaps 'mh/prefix-help-map
      "b" 'helm-descbinds)
     :config
     (helm-descbinds-mode))))

;;; keybindings-layer.el ends here
