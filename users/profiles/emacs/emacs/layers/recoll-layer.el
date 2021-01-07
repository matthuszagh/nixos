;;; recoll-layer.el --- Recoll Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def recoll
  :presetup
  (:layer straight
   (straight-use-package 'helm-recoll))

  :setup
  ;; TODO I can't think of a setup stage for this. Does this mean
  ;; setup should not be necessary?
  (message "recoll layer")

  :postsetup
  (:layer helm
   (use-package helm-recoll
     :config
     (helm-recoll-create-source "library" "~/.recoll/library"))))

;;; recoll-layer.el ends here
