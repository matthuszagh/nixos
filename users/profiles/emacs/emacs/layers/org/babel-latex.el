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

  (defun mh//org-src-block-res-fname ()
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

  (defun mh//org-src-block-latex-file-desc ()
    ""
    (let* ((elem (org-element-at-point))
           (clickablep (mh//header-match elem ":_clickable"))
           (backend org-export-current-backend))
      (if (and clickablep (string= backend "html"))
          (concat "file:" (mh//org-src-block-res-fname))
        nil)))

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
                     (mh//org-src-block-res-fname)))
          (:file-desc . (lambda ()
                          (mh//org-src-block-latex-file-desc)))
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
      (eval ret)))

  ;;   (setq org-latex-img-process
  ;;         '(("svg" . ("inkscape --pdf-poppler %f -T -l -o %O"))))

  ;;   (defun mh//org-babel-execute:latex (body params)
  ;;     "Execute a block of Latex code with Babel.
  ;; This function is called by `org-babel-execute-src-block'."
  ;;     (setq body (org-babel-expand-body:latex body params))
  ;;     (if (cdr (assq :file params))
  ;;         (let* ((out-file (cdr (assq :file params)))
  ;;                (extension (file-name-extension out-file))
  ;; 	       (tex-file (org-babel-temp-file "latex-" ".tex"))
  ;; 	       (border (cdr (assq :border params)))
  ;; 	       (imagemagick (cdr (assq :imagemagick params)))
  ;; 	       (im-in-options (cdr (assq :iminoptions params)))
  ;; 	       (im-out-options (cdr (assq :imoutoptions params)))
  ;; 	       (fit (or (cdr (assq :fit params)) border))
  ;; 	       (height (and fit (cdr (assq :pdfheight params))))
  ;; 	       (width (and fit (cdr (assq :pdfwidth params))))
  ;; 	       (headers (cdr (assq :headers params)))
  ;; 	       (in-buffer (not (string= "no" (cdr (assq :buffer params)))))
  ;; 	       (org-latex-packages-alist
  ;; 	        (append (cdr (assq :packages params)) org-latex-packages-alist)))
  ;;           (if (not (file-exists-p out-file))
  ;;               (progn
  ;;                 (with-temp-file tex-file
  ;;                   (let* ((split-body (s-split-up-to
  ;;                                       "\n" body 1))
  ;;                          (doc-str (car split-body))
  ;;                          (rest-str (cadr split-body)))
  ;;                     (insert doc-str)
  ;;                     (insert (concat
  ;;                              "\\definecolor{fg}{rgb}{"
  ;;                              (by-backend '((html . "0,0,0")
  ;;                                            (t . (org-latex-color :foreground))))
  ;;                              "}\n"))
  ;;                     (insert (concat
  ;;                              "\\definecolor{bg}{rgb}{"
  ;;                              (by-backend '((html . "1,1,1")
  ;;                                            (t . (org-latex-color :background))))
  ;;                              "}\n"))
  ;;                     (insert (concat
  ;;                              "\\def\\pc{"
  ;;                              (by-backend '((html . "100")
  ;;                                            (t . "20")))
  ;;                              "}\n"))
  ;;                     ;; TODO wrap in conditional on class
  ;;                     ;; (insert (concat
  ;;                     ;;          "\\def\\eqnBoxCol{gray!30!bg}\n"
  ;;                     ;;          "\\tikzset{emphBox/.style={draw=\\eqnBoxCol, fill=\\eqnBoxCol, "
  ;;                     ;;          "thick, rectangle, inner sep=5pt, inner ysep=10pt}}\n"
  ;;                     ;;          "\\ctikzset{-o/.style = {bipole nodes={none}{ocirc, fill=bg}}}\n"
  ;;                     ;;          "\\ctikzset{o-/.style = {bipole nodes={ocirc, fill=bg}{none}}}\n"
  ;;                     ;;          "\\ctikzset{o-o/.style = {bipole nodes={ocirc, fill=bg}{ocirc, fill=bg}}}\n"
  ;;                     ;;          "\\ctikzset{*-o/.style = {bipole nodes={circ}{ocirc, fill=bg}}}\n"
  ;;                     ;;          "\\ctikzset{o-*/.style = {bipole nodes={ocirc, fill=bg}{circ}}}\n"))
  ;;                     (insert rest-str)))
  ;;                 (let ((tmp-pdf (org-babel-latex-tex-to-pdf tex-file)))
  ;;                   (let* ((log-buf (get-buffer-create "*Org Babel LaTeX Output*"))
  ;;                          (err-msg "org babel latex failed")
  ;;                          (img-out (org-compile-file
  ;; 	                           tmp-pdf
  ;;                                    (cdr (assoc "svg" org-latex-img-process))
  ;;                                    extension err-msg log-buf)))
  ;;                     (shell-command (format "mv %s %s" img-out out-file))))))
  ;;           nil)) ;; signal that output has already been written to file
  ;;     body)

  ;;   (advice-add 'org-babel-execute:latex :override #'mh//org-babel-execute:latex)
  )

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

;;; babel-latex.el ends here
