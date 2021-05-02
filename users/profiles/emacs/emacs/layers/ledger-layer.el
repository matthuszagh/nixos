;;; ledger-layer.el --- Ledger Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def ledger
  :presetup
  (:layer straight
   (straight-use-package 'ledger-mode)
   (straight-use-package 'flycheck-ledger)
   (straight-use-package 'evil-ledger))

  :setup
  (use-package ledger-mode)

  :postsetup
  (:layer flycheck
   (use-package flycheck-ledger))
  (:layer modal-interactin
   (use-package evil-ledger
     :hook (ledger-mode . evil-ledger-mode))))
;;; ledger-layer.el ends here
