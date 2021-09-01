;;; librarian-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def librarian
  :presetup
  (:layer (straight helm)
   (straight-use-package '(helm-librarian
                           :host github
                           :repo "matthuszagh/helm-librarian")))

  :setup
  ;; TODO
  (message "librarian layer")

  :postsetup
  (:layer helm
   (use-package helm-librarian
     :config
     (setq librarian-executable "~/src/librarian/target/release/librarian")
     (setq librarian-library-directory "~/doc/library")))

  (:layer (helm modal)
   (general-define-key
    :keymaps 'mh/prefix-file-map
    "l" 'helm-librarian)))


;;; librarian-layer.el ends here
