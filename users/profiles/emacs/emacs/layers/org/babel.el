;;; babel.el --- Org Babel -*- lexical-binding: t; -*-

;;; Commentary:

;; Customize org behavior regarding source code.

;;; Code:

(defun mh//eval-default-headers (headers)
  ""
  (let ((lst nil))
    (dolist (elem (eval headers t))
      (if (listp (cdr elem))
          (push `(,(car elem) . ,(funcall (cdr elem))) lst)
        (push elem lst)))
    lst))

(org-babel-lob-ingest (concat user-emacs-directory "layers/org/lob.org"))

(load (concat user-emacs-directory "layers/org/babel-latex.el"))

;;; babel.el ends here
