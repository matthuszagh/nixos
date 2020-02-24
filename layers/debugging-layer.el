;;; debugging-layer.el --- Summary -*-lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def debugging
  :setup
  (use-package realgud)

  (use-package debbugs)

  ;; Source level debugging for Elisp. Built-in.
  (use-package edebug
    :config
    ;; Display full result of eval
    (setq edebug-print-length nil)

    ;; Edebug a defun or defmacro
    (defvar mh/fns-in-edebug nil
      "List of functions for which `edebug' is instrumented.")

    (defconst mh/fns-regexp (concat "(\\s-*"
                                    "\\(defun\\|defmacro\\)\\s-+"
                                    "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
      "Regexp to find defun or defmacro definition.")

    (defun mh/toggle-edebug-defun ()
      (interactive)
      (let (fn)
        (save-excursion
          (search-backward-regexp mh/fns-regexp)
          (setq fn (match-string 1))
          (mark-sexp)
          (narrow-to-region (point) (mark))
          (if (member fn mh/fns-in-edebug)
              ;; If the function is already being edebugged, uninstrument it
              (progn
                (setq mh/fns-in-edebug (delete fn mh/fns-in-edebug))
                (eval-region (point) (mark))
                (setq-default eval-expression-print-length 12)
                (setq-default eval-expression-print-level  4)
                (message "Edebug disabled: %s" fn))
            ;; If the function is not being edebugged, instrument it
            (progn
              (add-to-list 'mh/fns-in-edebug fn)
              (setq-default eval-expression-print-length nil)
              (setq-default eval-expression-print-level  nil)
              (edebug-defun)
              (message "Edebug: %s" fn)))
          (widen)))))

  :postsetup
  (:layer python
   (setq realgud:pdb-command-name "python3 -m pdb")))

;;; debugging-layer.el ends here
