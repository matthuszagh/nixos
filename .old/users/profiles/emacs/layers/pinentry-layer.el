;;; pinentry-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def pinentry
  :setup
  ;; prompt gpg passphrase in minibuffer instead of in new window.
  (setq epa-pinentry-mode 'loopback))

;;; pinentry-layer.el ends here
