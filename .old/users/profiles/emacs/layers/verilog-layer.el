;;; verilog-layer.el --- Verilog Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def verilog
  :setup
  (use-package verilog-mode
    :mode "\\.[st]*v[hp]*\\'"
    ;; :hook ((lambda ()
    ;;          (setq-local paragraph-start "[ 	]*\\(//+\\|\\**\\)[ 	]*$\\|^\f")))
    :bind (:map verilog-mode-map
           ("C-c C-f" . indent-buffer))
    :config
    ;; Turns off automatic newline after typing a semicolon, which is annoying.
    (setq verilog-auto-newline nil
          verilog-indent-level 3
          verilog-indent-level-declaration 3
          verilog-indent-level-behavioral 3
          verilog-indent-level-module 3
          verilog-auto-lineup 'all
	  verilog-align-ifelse t
	  verilog-highlight-modules t
	  verilog-auto-read-includes t
          verilog-case-indent 0
          indent-tabs-mode t
          verilog-auto-delete-trailing-whitespace t))

  ;; disable end of block comments. Although superficially nice, these
  ;; are difficult to keep in sync with the code
  (setq verilog-auto-endcomments nil)

  ;; use verilator for linting
  (setq verilog-linter "verilator –lint-only")

  :postsetup
  (:layer flycheck
   (setq-default flycheck-verilog-verilator-executable "verilator_bin"))

  (:layer modal
   (localleader :keymaps 'verilog-mode-map
     "c" (lambda (cmd)
	   (interactive
	    (list
	     (compilation-read-command compile-command)))
	   (compile cmd t))))

  (:layer (modal dumb-jump)
   (general-define-key
    :keymaps 'verilog-mode-map
    :states 'normal
    "g d" 'dumb-jump-go
    "g p" 'dumb-jump-back)))

;;; verilog-layer.el ends here
