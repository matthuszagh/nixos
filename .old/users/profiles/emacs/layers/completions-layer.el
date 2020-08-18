;;; completions-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def completions
  :presetup
  (:layer straight
   (straight-use-package 'company))

  :setup
  (use-package company
    :config
    ;; show completions immediately
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-show-numbers t)
    (global-company-mode)
    ;; Maintain case information for completions.
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case nil)
    ;; Default backends.
    (setq company-backends '(company-files
                             company-keywords
                             company-capf
                             ;; company-dabbrev
                             company-dabbrev-code)))

  :postsetup
  (:layer keybinding-management
   ;; (general-def company-active-map
   ;;   "<tab>" 'company-complete-selection)
   (general-def company-active-map
     "<tab>" 'company-complete-common)
   )
  (:layer lsp
   (use-package company-lsp
     :config
     (add-to-list 'company-backends 'company-lsp))))

;;; completions-layer.el ends here
