;;; ledger-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def ledger
  :setup
  (use-package ledger-mode)

  :postsetup
  (:layer flycheck
   (use-package flycheck-ledger))
  (:layer modal-interactin
   (use-package evil-ledger
     :hook (ledger-mode . evil-ledger-mode))))
;;; ledger-layer.el ends here
