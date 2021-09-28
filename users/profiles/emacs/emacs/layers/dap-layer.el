;;; dap-layer.el --- Debug Adapter Protocol Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def dap
  :presetup
  (:layer straight
   (straight-use-package 'dap-mode))

  :setup
  (use-package dap-mode
    :config
    (dap-mode 1)
    (dap-ui-mode 1))

  (use-package dap-lldb
    :after dap-mode)

  (use-package dap-python
    :after dap-mode)

  (use-package dap-gdb-lldb
    :after dap-mode
    :config
    (dap-gdb-lldb-setup))

  :postsetup
  (:layer rust
   (dap-register-debug-template "Rust::GDB Run Configuration"
                                (list :type "gdb"
                                      :request "launch"
                                      :name "GDB::Run"
                                      :gdbpath "rust-gdb"
                                      :target nil
                                      :cwd nil))))

;;; dap-layer.el ends here
