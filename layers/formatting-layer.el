;;; formatting-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def formatting
  :presetup
  (:layer straight
          (straight-use-package 'aggressive-indent))

  :setup
  (use-package aggressive-indent)
  ;; :config
  ;; (global-aggressive-indent-mode 1))

  :postsetup
  (:layer keybinding-management
          (general-def mh/prefix-format-map
            "c" 'comment-or-uncomment-region
            "a" 'align-regexp))
  ;;(:layer notes
  ;;      (add-to-list 'org-mode aggressive-indent-excluded-modes))
  )

;;; formatting-layer.el ends here
