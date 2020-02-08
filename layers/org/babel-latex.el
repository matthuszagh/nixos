;;; babel-latex.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ox-latex
  :config
  (setq org-babel-default-header-args:latex
        `((:exports . "results")
          (:results . "output file drawer")))

  (setq org-html-with-latex 'luasvgm)

  ;; (defun mh//org-html-export-link-filter (link backend channel)
  ;;   (when (org-export-derived-backend-p backend 'html)
  ;;     (concat "/" link)))

  ;; (add-to-list 'org-export-filter-link-functions
  ;;              'mh//org-html-export-link-filter)

  (defun mh//org-html-format-latex (latex-frag processing-type info)
    "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It can
be `mathjax', `verbatim', nil, t or symbols in
`org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
`imagemagick'.  See `org-html-with-latex' for more information.
INFO is a plist containing export properties."
    (let ((cache-relpath "") (cache-dir ""))
      (unless (eq processing-type 'mathjax)
        (let ((bfn (or (buffer-file-name)
		       (make-temp-name
		        (expand-file-name "latex" temporary-file-directory))))
	      (latex-header
	       (let ((header (plist-get info :latex-header)))
	         (and header
		      (concat (mapconcat
			       (lambda (line) (concat "#+LATEX_HEADER: " line))
			       (org-split-string header "\n")
			       "\n")
			      "\n")))))
	  (setq cache-relpath
	        (concat (file-name-as-directory org-preview-latex-image-directory)
		        (file-name-sans-extension
		         (file-name-nondirectory bfn)))
	        cache-dir (file-name-directory bfn))
	  ;; Re-create LaTeX environment from original buffer in
	  ;; temporary buffer so that dvipng/imagemagick can properly
	  ;; turn the fragment into an image.
	  (setq latex-frag (concat latex-header latex-frag))))
      (with-temp-buffer
        (insert latex-frag)
        (org-format-latex cache-relpath nil nil cache-dir nil
			  "Creating LaTeX Image..." nil processing-type)
        (replace-regexp-in-string "\\[\\[file:" "[[file:/" (buffer-string)))))

  (advice-add 'org-html-format-latex :override #'mh//org-html-format-latex)

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

  (setq org-latex-img-process
        '(("svg" . ("dvisvgm --pdf %f -n -b min -c 10 -o %O"))))

  (defun mh//org-babel-execute:latex (body params)
    "Execute a block of Latex code with Babel.
This function is called by `org-babel-execute-src-block'."
    (setq body (org-babel-expand-body:latex body params))
    (if (cdr (assq :file params))
        (let* ((out-file (cdr (assq :file params)))
	       (extension (file-name-extension out-file))
	       (tex-file (org-babel-temp-file "latex-" ".tex"))
	       (border (cdr (assq :border params)))
	       (imagemagick (cdr (assq :imagemagick params)))
	       (im-in-options (cdr (assq :iminoptions params)))
	       (im-out-options (cdr (assq :imoutoptions params)))
	       (fit (or (cdr (assq :fit params)) border))
	       (height (and fit (cdr (assq :pdfheight params))))
	       (width (and fit (cdr (assq :pdfwidth params))))
	       (headers (cdr (assq :headers params)))
	       (in-buffer (not (string= "no" (cdr (assq :buffer params)))))
	       (org-latex-packages-alist
	        (append (cdr (assq :packages params)) org-latex-packages-alist)))
          (cond
           ((and (string-suffix-p ".png" out-file) (not imagemagick))
            (org-create-formula-image
             body out-file org-format-latex-options in-buffer))
           ((string-suffix-p ".tikz" out-file)
	    (when (file-exists-p out-file) (delete-file out-file))
	    (with-temp-file out-file
	      (insert body)))
           ((and (not imagemagick)
                 (assoc extension org-latex-img-process))
            (if (not (file-exists-p out-file))
                (progn
                  (with-temp-file tex-file
                    (insert body))
                  (let ((tmp-pdf (org-babel-latex-tex-to-pdf tex-file)))
                    (let* ((log-buf (get-buffer-create "*Org Babel LaTeX Output*"))
                           (err-msg "org babel latex failed")
                           (img-out (org-compile-file
	                             tmp-pdf
                                     (cdr (assoc "svg" org-latex-img-process))
                                     extension err-msg log-buf)))
                      (shell-command (format "mv %s %s" img-out out-file)))))))
	   ((and (or (string= "svg" extension)
		     (string= "html" extension))
	         (executable-find org-babel-latex-htlatex))
	    ;; TODO: this is a very different way of generating the
	    ;; frame latex document than in the pdf case.  Ideally, both
	    ;; would be unified.  This would prevent bugs creeping in
	    ;; such as the one fixed on Aug 16 2014 whereby :headers was
	    ;; not included in the SVG/HTML case.
	    (with-temp-file tex-file
	      (insert (concat
		       "\\documentclass[preview]{standalone}
\\def\\pgfsysdriver{pgfsys-tex4ht.def}
"
		       (mapconcat (lambda (pkg)
				    (concat "\\usepackage" pkg))
				  org-babel-latex-htlatex-packages
				  "\n")
		       (if headers
			   (concat "\n"
				   (if (listp headers)
				       (mapconcat #'identity headers "\n")
				     headers) "\n")
		         "")
		       "\\begin{document}"
		       body
		       "\\end{document}")))
	    (when (file-exists-p out-file) (delete-file out-file))
	    (let ((default-directory (file-name-directory tex-file)))
	      (shell-command (format "%s %s" org-babel-latex-htlatex tex-file)))
	    (cond
	     ((file-exists-p (concat (file-name-sans-extension tex-file) "-1.svg"))
	      (if (string-suffix-p ".svg" out-file)
		  (progn
		    (shell-command "pwd")
		    (shell-command (format "mv %s %s"
					   (concat (file-name-sans-extension tex-file) "-1.svg")
					   out-file)))
	        (error "SVG file produced but HTML file requested")))
	     ((file-exists-p (concat (file-name-sans-extension tex-file) ".html"))
	      (if (string-suffix-p ".html" out-file)
		  (shell-command "mv %s %s"
			         (concat (file-name-sans-extension tex-file)
				         ".html")
			         out-file)
	        (error "HTML file produced but SVG file requested")))))
	   ((or (string= "pdf" extension) imagemagick)
	    (with-temp-file tex-file
	      (require 'ox-latex)
	      (insert
	       (org-latex-guess-inputenc
	        (org-splice-latex-header
	         org-format-latex-header
	         (delq
		  nil
		  (mapcar
		   (lambda (el)
		     (unless (and (listp el) (string= "hyperref" (cadr el)))
		       el))
		   org-latex-default-packages-alist))
	         org-latex-packages-alist
	         nil))
	       (if fit "\n\\usepackage[active, tightpage]{preview}\n" "")
	       (if border (format "\\setlength{\\PreviewBorder}{%s}" border) "")
	       (if height (concat "\n" (format "\\pdfpageheight %s" height)) "")
	       (if width  (concat "\n" (format "\\pdfpagewidth %s" width))   "")
	       (if headers
		   (concat "\n"
			   (if (listp headers)
			       (mapconcat #'identity headers "\n")
			     headers) "\n")
	         "")
	       (if fit
		   (concat "\n\\begin{document}\n\\begin{preview}\n" body
			   "\n\\end{preview}\n\\end{document}\n")
	         (concat "\n\\begin{document}\n" body "\n\\end{document}\n"))))
            (when (file-exists-p out-file) (delete-file out-file))
	    (let ((transient-pdf-file (org-babel-latex-tex-to-pdf tex-file)))
	      (cond
	       ((string= "pdf" extension)
	        (rename-file transient-pdf-file out-file))
	       (imagemagick
	        (org-babel-latex-convert-pdf
	         transient-pdf-file out-file im-in-options im-out-options)
	        (when (file-exists-p transient-pdf-file)
		  (delete-file transient-pdf-file)))
	       (t
	        (error "Can not create %s files, please specify a .png or .pdf file or try the :imagemagick header argument"
		       extension))))))
          nil) ;; signal that output has already been written to file
      body))

  (advice-add 'org-babel-execute:latex :override #'mh//org-babel-execute:latex))

(provide 'babel-latex)
;;; babel-latex.el ends here
