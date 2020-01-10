;;; make-layer.el --- Make Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def make
  :setup
  (use-package compile
    :config
    (setq compilation-scroll-output 'next-error)
    (setq compilation-skip-threshold 2)
    (defun mh/toggle-comint-compilation ()
      "Restart compilation with (or without) `comint-mode'."
      (interactive)
      (cl-callf (lambda (mode) (if (eq mode t) nil t))
          (elt compilation-arguments 1))
      (recompile))
    ;; :bind (("C-c C-c" . (lambda (cmd)
    ;;     		  (interactive
    ;;     		   (list
    ;;     		    (compilation-read-command compile-command)))
    ;;     		  (compile cmd t)))
    ;;        :map compilation-mode-map
    ;;        ("C-c i" . mh/toggle-comint-compilation)
    ;;        :map compilation-minor-mode-map
    ;;        ("C-c i" . mh/toggle-comint-compilation)
    ;;        :map compilation-shell-minor-mode-map
    ;;        ("C-c i" . mh/toggle-comint-compilation)))
    )

  :postsetup
  (:layer modal
   (localleader :keymaps 'makefile-mode-map
     "c" '(lambda (cmd)
            (interactive
             (list
              (compilation-read-command compile-command)))
            (compile cmd t)))

   (localleader :keymaps 'compilation-mode-map
     "i" 'mh/toggle-comint-compilation)))

;;; make-layer.el ends here
