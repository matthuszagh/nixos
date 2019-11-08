;;; rmsbolt-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def rmsbolt
  :depends (programming)

  :setup
  (use-package rmsbolt
    :config
    (setq rmsbolt-asm-format nil))

  :postsetup
  (:layer c
   (add-hook 'c-mode-common-hook 'rmsbolt-mode)
   (add-hook 'c-mode-common-hook
             (lambda ()
               (setq-local rmsbolt-command "clang -O0 -target riscv64"))))

  (:layer (c modal)
   (localleader :keymaps 'c-mode-base-map
     "C" 'rmsbolt-compile))

  (:layer elisp
   (add-hook 'emacs-lisp-mode-hook 'rmsbolt-mode))
  (:layer (elisp modal)
   (localleader :keymaps 'elisp-mode-map
     "C" 'rmsbolt-compile))

  (:layer python
   (add-hook 'python-mode-hook 'rmsbolt-mode))
  (:layer (python modal)
   (localleader :keymaps 'python-mode-map
     "C" 'rmsbolt-compile))

  (:layer rust
   (add-hook 'rust-mode-hook 'rmsbolt-mode))
  (:layer (rust modal)
   (localleader :keymaps 'rust-mode-map
     "C" 'rmsbolt-compile)))

;;; rmsbolt-layer.el ends here
