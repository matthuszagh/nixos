;;; yapfify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "yapfify" "yapfify.el" (23298 5509 663991 65000))
;;; Generated autoloads from yapfify.el

(autoload 'yapfify-region "yapfify" "\
Try to yapfify the current region.

If yapf exits with an error, the output will be shown in a help-window.

\(fn BEGINNING END)" t nil)

(autoload 'yapfify-buffer "yapfify" "\
Yapfify whole buffer.

\(fn)" t nil)

(autoload 'yapf-mode "yapfify" "\
Automatically run YAPF before saving.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; yapfify-autoloads.el ends here
