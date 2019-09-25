;;; buffers-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def buffers
  :setup
  (global-auto-revert-mode t)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

;;; buffers-layer.el ends here
