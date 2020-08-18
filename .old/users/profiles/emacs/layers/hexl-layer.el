;;; hexl-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def hexl
  :setup
  (use-package hexl)

  :postsetup
  (:layer modal
   (localleader :keymaps 'hexl-mode-map
     "a" 'hexl-goto-address)))

;;; hexl-layer.el ends here
