;;; calc-layer.el --- Calc Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def calc
  :setup
  (use-package calc
    :config
    ;; always use two's complement
    ;; (setq calc-twos-complement-mode t)
    )

  :postsetup
  (:layer modal
   (general-def mh/prefix-calc-map
     "n" 'calc
     "y" 'calc-grab-region)

   (general-define-key
    :keymaps 'calc-mode-map
    :states 'normal
    "DEL" 'calc-pop)))

;;; calc-layer.el ends here
