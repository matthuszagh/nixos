;;; time-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def time
  :setup
  ;; enable display-time-mode by default
  (setq display-time-mode t)
  ;; use 24h time
  (setq display-time-24hr-format t))

;;; time-layer.el ends here
