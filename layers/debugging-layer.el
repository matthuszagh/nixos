;;; debugging-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def debugging
  :setup
  (use-package realgud)

  (use-package debbugs)

  :postsetup
  (:layer python
   (setq realgud:pdb-command-name "python3 -m pdb")))

;;; debugging-layer.el ends here
