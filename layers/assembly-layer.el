;;; assembly-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def assembly
  :depends (programming)

  :presetup
  (:layer straight
          (straight-use-package 'x86-lookup))

  :setup
  ;; Lookup Intel assembly mnemonics in Intel documentation.
  (use-package x86-lookup
    :config
    (setq x86-lookup-pdf "~/doc/library/computing/software/assembly/Intel Software Developer’s Manual (2017).pdf")))

;;; assembly-layer.el ends here
