;;; jupyter-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def jupyter
  :presetup
  (:layer straight
   (straight-use-package 'ein))

  :setup
  (use-package ein
    :config
    (setq ein:output-area-inlined-images t)
    (setq my-jupyter-start-dir "/home/matt/.jupyter")
    (setq ein:jupyter-server-notebook-directory "~/.jupyter/")))

;;; jupyter-layer.el ends here
