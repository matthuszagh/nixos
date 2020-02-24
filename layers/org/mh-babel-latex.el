;;; mh-babel-latex.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)

(use-package ox-latex
  :config
  (setq org-babel-default-header-args:latex
        `((:exports . "results")
          (:results . "file link replace")
          (:wrap . "results")
          (:cache . "yes")
          (:file . (lambda ()
                     (concat "tmp/"
                             (sha1 (org-element-property :value (org-element-at-point)))
                             (by-backend '((html . "-html") (t . "-org")))
                             ".svg")))
          (:post . "attr_wrap(htmlwidth=\"100%\", orgwidth=\"1000\", data=*this*)")))

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
          (if (not (file-exists-p out-file))
              (progn
                (with-temp-file tex-file
                  (let* ((split-body (s-split-up-to
                                      "\n" body 1))
                         (doc-str (car split-body))
                         (rest-str (cadr split-body)))
                    (insert doc-str)
                    (insert (concat
                             "\\definecolor{fg}{rgb}{"
                             (by-backend '((html . "0,0,0")
                                           (t . (org-latex-color :foreground))))
                             "}\n"))
                    (insert (concat
                             "\\definecolor{bg}{rgb}{"
                             (by-backend '((html . "1,1,1")
                                           (t . (org-latex-color :background))))
                             "}\n"))
                    (insert (concat
                             "\\def\\pc{"
                             (by-backend '((html . "100")
                                           (t . "20")))
                             "}\n"))
                    (insert (concat
                             "\\def\\eqnBoxCol{gray!30!bg}\n"
                             "\\tikzset{emphBox/.style={draw=\\eqnBoxCol, fill=\\eqnBoxCol, "
                             "thick, rectangle, inner sep=5pt, inner ysep=10pt}}\n"
                             "\\ctikzset{-o/.style = {bipole nodes={none}{ocirc, fill=bg}}}\n"
                             "\\ctikzset{o-/.style = {bipole nodes={ocirc, fill=bg}{none}}}\n"
                             "\\ctikzset{o-o/.style = {bipole nodes={ocirc, fill=bg}{ocirc, fill=bg}}}\n"
                             "\\ctikzset{*-o/.style = {bipole nodes={circ}{ocirc, fill=bg}}}\n"
                             "\\ctikzset{o-*/.style = {bipole nodes={ocirc, fill=bg}{circ}}}\n"))
                    (insert rest-str)))
                (let ((tmp-pdf (org-babel-latex-tex-to-pdf tex-file)))
                  (let* ((log-buf (get-buffer-create "*Org Babel LaTeX Output*"))
                         (err-msg "org babel latex failed")
                         (img-out (org-compile-file
	                           tmp-pdf
                                   (cdr (assoc "svg" org-latex-img-process))
                                   extension err-msg log-buf)))
                    (shell-command (format "mv %s %s" img-out out-file))))))
          nil)) ;; signal that output has already been written to file
    body)

  (advice-add 'org-babel-execute:latex :override #'mh//org-babel-execute:latex))

(defun mh/update-eqn-numbers-in-section ()
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

(provide 'mh-babel-latex)
;;; mh-babel-latex.el ends here
