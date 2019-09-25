;;; octave-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def octave
  :setup
  (use-package octave
    :mode ("\\.m\\'" . octave-mode)
    :hook (inferior-octave-mode . (lambda ()
                                    (setq-local fill-column nil)))
    :config
    (add-to-list 'same-window-buffer-names "*Inferior Octave*"))

  :postsetup
  (:layer modal
   (localleader 'octave-mode-map
     "h" 'octave-help)))

;;; octave-layer.el ends here
