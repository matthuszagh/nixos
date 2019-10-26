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

  (:layer elisp
   (add-hook 'emacs-lisp-mode-hook 'rmsbolt-mode))

  (:layer python
   (add-hook 'python-mode-hook 'rmsbolt-mode))

  (:layer rust
   (add-hook 'rust-mode-hook 'rmsbolt-mode)))

;;; rmsbolt-layer.el ends here
