;;; babel-latex.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)

;; TODO customizations if patch accepted
(defun latex-preamble-by-backend (params)
  (concat "\\documentclass{"
          (cdr (assoc :_class params))
          "}"
          "\\definecolor{fg}{rgb}{"
          (by-backend '((html . "0,0,0")
                        (t . (org-latex-color :foreground))))
          "}\n"
          "\\definecolor{bg}{rgb}{"
          (by-backend '((html . "1,1,1")
                        (t . (org-latex-color :background))))
          "}\n"
          "\\def\\pc{"
          (by-backend '((html . "100")
                        (t . "20")))
          "}\n"
          (if (string= "tikz" (cdr (assoc :_class params)))
              (concat "\\def\\eqnBoxCol{gray!30!bg}\n"
                      "\\tikzset{emphBox/.style={draw=\\eqnBoxCol, fill=\\eqnBoxCol, "
                      "thick, rectangle, inner sep=5pt, inner ysep=10pt}}\n"
                      "\\ctikzset{-o/.style = {bipole nodes={none}{ocirc, fill=bg}}}\n"
                      "\\ctikzset{o-/.style = {bipole nodes={ocirc, fill=bg}{none}}}\n"
                      "\\ctikzset{o-o/.style = {bipole nodes={ocirc, fill=bg}{ocirc, fill=bg}}}\n"
                      "\\ctikzset{*-o/.style = {bipole nodes={circ}{ocirc, fill=bg}}}\n"
                      "\\ctikzset{o-*/.style = {bipole nodes={ocirc, fill=bg}{circ}}}\n"))))

(setq org-babel-latex-preamble
      (lambda (params)
        (latex-preamble-by-backend params)))

(setq org-babel-latex-begin-env
      (lambda (_)
        "\\begin{document}{\\color{fg}"))

(setq org-babel-latex-end-env
      (lambda (_)
        "}\\end{document}"))

(use-package ox-latex
  :config
  (defun mh//concat-list (list)
    (let ((res ""))
      (dolist (elem list)
        (setq res (concat res elem)))
      res))

  (defun mh//header-match (elem match)
    "Returns `t' if MATCH is explicitly a header of ELEM.
Explicit in this context means that it's literally written in the
file rather than being provided as a default header argument."
    (let* ((elem (org-element-at-point))
           (elem-header (append (list (org-element-property :parameters elem))
                                (org-element-property :header elem)))
           (matchp nil))
      (dolist (elt elem-header)
        (if (string-match match elt)
            (setq matchp t)))
      matchp))

  (defun mh//org-src-block-contents ()
    (let ((elem (org-element-at-point)))
      (concat (org-element-property :value elem)
              (mh//concat-list (org-element-property :header elem)))))

  (defun mh//org-src-block-result-filename ()
    (let* ((elem (org-element-at-point))
           (class-mathp (mh//header-match elem ":_class math")))
      (if (and org-export-current-backend class-mathp)
          nil
        (concat "tmp/"
        	(sha1 (mh//org-src-block-contents))
                (by-backend '((html . "-html") (t . "-org")))
                ".svg"))))

  (defun mh//org-src-block-latex-post ()
    ""
    (let* ((elem (org-element-at-point))
           (class-mathp (mh//header-match elem ":_class math"))
           (wrap-str "attr_wrap(htmlwidth=\"100%\", orgwidth=\"1000\", name=\"\", data=*this*)")
           (backend org-export-current-backend))
      (if backend
          ;; for latex, we still want to wrap the caption if it's a
          ;; figure
          (if (string= backend "latex")
              (if class-mathp
                  nil
                wrap-str)
            (if (and (string= backend "html") class-mathp)
                "latexml_proc(data=*this*)"
              wrap-str))
        wrap-str)))

  (defun mh//org-src-block-latex-results ()
    ""
    (let* ((elem (org-element-at-point))
           (class-mathp (mh//header-match elem ":_class math")))
      (if org-export-current-backend
          (let ((backend org-export-current-backend))
            (if (or (string= backend "latex")
                    (and (string= backend "html") class-mathp))
                "latex"
              "file link replace"))
        "file link replace")))

  (defun mh//org-src-block-latex-file-description ()
    ""
    (let* ((elem (org-element-at-point))
           (clickablep (mh//header-match elem ":_clickable"))
           (backend org-export-current-backend))
      (if (and clickablep (string= backend "html"))
          (concat "file:" (mh//org-src-block-result-filename))
        [])))

  (defun mh//org-src-block-latex-wrap ()
    (let* ((elem (org-element-at-point))
           (class-mathp (mh//header-match elem ":_class math"))
           (backend org-export-current-backend))
      (if backend
          (if (string= backend "latex")
              nil
            (if (and (string= backend "html") class-mathp)
                nil
              "results"))
        "results")))

  (setq org-babel-default-header-args:latex
        `((:exports . "results")
          (:results . (lambda ()
                        (mh//org-src-block-latex-results)))
          (:wrap . (lambda ()
                     (mh//org-src-block-latex-wrap)))
          (:cache . "yes")
          (:file . (lambda ()
                     (mh//org-src-block-result-filename)))
          (:file-desc . (lambda ()
                          (mh//org-src-block-latex-file-description)))
          (:post . (lambda ()
                     (mh//org-src-block-latex-post)))))

  (setq org-html-with-latex 'html)
  (setq org-latex-to-html-convert-command
        "latexmlc 'literal:%i' --profile=math --preload=siunitx.sty 2>/dev/null | head -c -1")

  (defun by-backend (blist)
    (let ((ret nil))
      (if org-export-current-backend
          (let* ((backend-name org-export-current-backend)
                 (elem (assoc backend-name blist)))
            (if elem
                (setq ret (cdr elem))))
        (let ((elem (assoc t blist)))
          (setq ret (cdr elem))))
      (eval ret))))

(defun mh/update-eqn-numbers-in-section ()
  ""
  (interactive)
  (let ((beg (if (org-before-first-heading-p) (point-min)
	       (save-excursion
		 (org-with-limited-levels (org-back-to-heading t) (point)))))
	(end (org-with-limited-levels (org-entry-end-position)))
        (count 1))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              (rx (or "\\tag" "number=")
                  "{"
                  (group (+ digit))
                  "}") end t)
        (replace-match (format "%d" count) nil nil nil 1)
        (setq count (1+ count))))))

(defun mh/org-in-latex-blockp ()
  "Indicate if we're inside a latex source block."
  (let ((elem (org-ml-parse-element-at (point))))
    (if (and (org-ml-is-type 'src-block elem)
             (string-equal (org-ml-get-property :language elem) "latex"))
        t
      nil)))

(use-package org-texnum)

;; TODO these are mostly identical to org-texnum functions. We should
;; be able to refactor this into a more general API.

(defun mh//convert-latex-blocks-for-latex-export ()
  "Convert LaTeX src blocks in the current buffer to export blocks."
  (let ((latex-blocks (org-texnum//latex-blocks-in-current-buffer)))
    (while (not (eq latex-blocks nil))
      (let* ((latex-block (car latex-blocks))
             (begin (org-ml-get-property :begin latex-block))
             (value (org-ml-get-property :value latex-block))
             (end (org-ml-get-property :end latex-block)))
        (goto-char begin)
        (delete-region begin end)
        (insert (concat "#+BEGIN_EXPORT latex\n"
                        value
                        "\n"
                        "#+END_EXPORT\n\n"))
        (setq latex-blocks (org-texnum//latex-blocks-in-current-buffer))))))

(defun mh//get-results-blocks-in-section (section)
  ""
  (let* ((special-blocks (--filter (org-ml-is-type 'special-block it) section))
         (results-blocks (--filter (string-equal "results" (org-ml-get-property :type it))
                                   special-blocks)))
    results-blocks))

(defun mh//results-blocks-in-headline (headline results-blocks)
  "Retrieve all RESULTS-BLOCKS in HEADLINE."
  (let ((section (org-texnum//get-section headline))
        (headlines (org-texnum//get-headlines headline)))
    (setq results-blocks
          (append results-blocks (mh//get-results-blocks-in-section section)))
    (dolist (headline headlines)
      (setq results-blocks
            (mh//results-blocks-in-headline headline results-blocks)))
    results-blocks))

(defun mh//results-blocks-in-current-buffer ()
  "Retrieve all results src blocks in the current buffer."
  (let* ((buffer-tree (org-ml-parse-this-buffer))
         (top-section (org-texnum//get-section buffer-tree))
         (results-blocks (mh//get-results-blocks-in-section top-section))
         (headlines (org-texnum//get-headlines buffer-tree)))
    (dolist (headline headlines)
      (setq results-blocks (mh//results-blocks-in-headline headline results-blocks)))
    results-blocks))

(defun mh//remove-results-blocks-in-current-buffer ()
  ""
  (let ((results-blocks (mh//results-blocks-in-current-buffer)))
    (while (not (eq results-blocks nil))
      (let* ((results-block (car results-blocks))
             (begin (org-ml-get-property :begin results-block))
             (end (org-ml-get-property :end results-block)))
        (goto-char begin)
        (delete-region begin end)
        (setq results-blocks (mh//results-blocks-in-current-buffer))))))

(defun mh//latex-export-remove-results-blocks (backend)
  "Remove results blocks.
BACKEND is the export backend."
  (when (org-export-derived-backend-p backend 'latex)
    (mh//remove-results-blocks-in-current-buffer)))

(defun mh//latex-export-latex-src-block-convert (backend)
  "Replace LaTeX src blocks with LaTeX export blocks.
BACKEND is the export backend."
  (when (org-export-derived-backend-p backend 'latex)
    (mh//convert-latex-blocks-for-latex-export)))

(add-hook 'org-export-before-processing-hook
          'mh//latex-export-remove-results-blocks)

(add-hook 'org-export-before-processing-hook
          'mh//latex-export-latex-src-block-convert)

;;; babel-latex.el ends here
