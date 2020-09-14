;;; internet-layer.el --- Internet Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def internet
  :presetup
  (:layer (straight helm)
   (straight-use-package 'helm-eww))

  :setup
  ;; eww normally reuses existing eww buffers. To open a new buffer,
  ;; use a prefix argument before calling eww.
  (use-package eww
    :config
    ;; use eww as default browser
    (setq browse-url-browser-function 'eww-browse-url)
    ;; use firefox as backup
    (setq browse-url-secondary-browser-function 'browse-url-firefox))

  (use-package shr
    :config
    ;; use monospaced rather than proportional fonts
    (setq shr-use-fonts nil)
    (setq shr-use-colors nil)
    ;; wrap text at 70 in eww
    (setq shr-width 70))

  :postsetup
  (:layer helm
   (use-package helm-eww))

  (:layer (helm modal)
   (localleader :keymaps 'eww-mode-map
     "H" 'helm-eww-history))

  (:layer modal
   (general-def mh/prefix-search-map
     "i" 'eww)
   (localleader :keymaps 'eww-mode-map
     "h" 'eww-back-url
     "l" 'eww-forward-url)))

;;; internet-layer.el ends here
