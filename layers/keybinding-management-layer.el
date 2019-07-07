;;; keybinding-management-layer.el -*-lexical-binding: t; -*-

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
  (use-package general)

  (use-package which-key
    :functions which-key-mode
    :config
    (which-key-mode))

  :postsetup
  (:layer helm
          (use-package helm-descbinds
            :after helm
            ;; TODO this should also depend on modal-interaction
            :general
            (:keymaps 'mh/prefix-help-map
                      "b" 'helm-describe-bindings)
            :config
            (helm-descbinds-mode))))

;;; keybindings-layer.el ends here
