;;; calc-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def calc
  :setup
  (use-package calc
    :config
    ;; always use two's complement
    (setq calc-twos-complement-mode t))

  :postsetup
  (:layer modal
   ;; (evil-set-initial-state 'calc-mode 'emacs)
   (general-def mh/prefix-map
     "n" 'calc)

   (general-define-key
    :keymaps 'calc-mode-map
    :states 'normal
    "DEL" 'calc-pop)))

;;; calc-layer.el ends here
