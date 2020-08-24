;;; prescient-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def prescient
  :depends (base no-littering)
  :presetup
  (:layer straight
   (straight-use-package 'prescient)
   (straight-use-package 'company-prescient))

  :setup
  (use-package prescient)

  :postsetup
  (:layer company
   (use-package company-prescient
     :config
     (company-prescient-mode))))


;;; prescient-layer.el ends here
