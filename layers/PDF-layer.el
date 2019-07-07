;;; pdf-layer.el -*-lexical-binding: t; -*-

;;; Code:

(layer-def pdf
  :presetup
  (:layer straight
          (straight-use-package 'pdf-tools))

  :setup
  (use-package pdf-tools
    :mode "\\.pdf\\'"
    :config
    ;; TODO we must currently run this for `pdf-tools' to work. However, this doesn't integrate very
    ;; well with Nix. Once I get nixpkgs working, I should try to get rid of this.
    (pdf-tools-install)

    (setq-default pdf-view-display-size 'fit-page)

    ;; Only warn when opening files larger than 1GB. This is particularly useful for PDFs which are
    ;; often larger than the default threshold.
    (setq large-file-warning-threshold 1000000000))
  )
;; :postsetup
;; (:layer modal-interaction
;;         (localleader :keymaps 'pdf-view-mode-map
;;          "j" 'pdf-view-next-line-or-next-page
;;          "k" 'pdf-view-previous-line-or-previous-page)))

;;; pdf-layer.el ends here
