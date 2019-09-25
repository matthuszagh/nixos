;;; internet-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def internet
  :presetup
  ;; TODO add helm to this layer list
  (:layer straight
   (straight-use-package 'helm-eww))

  :setup
  (use-package eww
    :config
    ;; use eww as default browser
    (setq browse-url-browser-function 'eww-browse-url))

  (use-package shr
    :config
    ;; use monospaced rather than proportional fonts
    (setq shr-use-fonts nil)
    ;; wrap text at 70 in eww
    (setq shr-width 70))

  :postsetup
  (:layer helm
   (use-package helm-eww)
   ;; TODO should also depend on modal interaction
   (localleader :keymaps 'eww-mode-map
     "H" 'helm-eww-history))
  (:layer modal
   (general-def mh/prefix-search-map
     "i" 'eww)
   (localleader :keymaps 'eww-mode-map
     "h" 'eww-back-url
     "l" 'eww-forward-url)))

;;; internet-layer.el ends here
