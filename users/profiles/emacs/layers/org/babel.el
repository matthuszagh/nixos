;;; babel.el --- Org Babel -*- lexical-binding: t; -*-

;;; Commentary:

;; Customize org behavior regarding source code.

;;; Code:

(defun mh//eval-default-headers (headers)
  (setq lst nil)
  (dolist (elem (eval headers t))
    (if (listp (cdr elem))
        (add-to-list 'lst `(,(car elem) . ,(funcall (cdr elem))))
      (add-to-list 'lst elem)))
  lst)

(defun mh/org-babel-get-src-block-info (&optional light datum)
  "Extract information from a source block or inline source block.

When optional argument LIGHT is non-nil, Babel does not resolve
remote variable references; a process which could likely result
in the execution of other code blocks, and do not evaluate Lisp
values in parameters.

By default, consider the block at point.  However, when optional
argument DATUM is provided, extract information from that parsed
object instead.

Return nil if point is not on a source block.  Otherwise, return
a list with the following pattern:

  (language body arguments switches name start coderef)"
  (let* ((datum (or datum (org-element-context)))
	 (type (org-element-type datum))
	 (inline (eq type 'inline-src-block)))
    (when (memq type '(inline-src-block src-block))
      (let* ((lang (org-element-property :language datum))
	     (lang-headers (intern
			    (concat "org-babel-default-header-args:" lang)))
	     (name (org-element-property :name datum))
	     (info
	      (list
	       lang
	       (org-babel--normalize-body datum)
	       (apply #'org-babel-merge-params
		      (if inline org-babel-default-inline-header-args
			org-babel-default-header-args)
		      (and (boundp lang-headers)
                           (mh//eval-default-headers lang-headers))
	              (append
		       ;; If DATUM is provided, make sure we get node
		       ;; properties applicable to its location within
		       ;; the document.
		       (org-with-point-at (org-element-property :begin datum)
		         (org-babel-params-from-properties lang light))
		       (mapcar (lambda (h)
			         (org-babel-parse-header-arguments h light))
			       (cons (org-element-property :parameters datum)
			             (org-element-property :header datum)))))
	       (or (org-element-property :switches datum) "")
	       name
	       (org-element-property (if inline :begin :post-affiliated)
				     datum)
	       (and (not inline) (org-src-coderef-format datum)))))
        (unless light
	  (setf (nth 2 info) (org-babel-process-params (nth 2 info))))
        (setf (nth 2 info) (org-babel-generate-file-param name (nth 2 info)))
        info))))

(advice-add 'org-babel-get-src-block-info :override #'mh/org-babel-get-src-block-info)

(defun mh/org-babel-insert-result (result &optional result-params info hash lang)
  "Insert RESULT into the current buffer.

By default RESULT is inserted after the end of the current source
block.  The RESULT of an inline source block usually will be
wrapped inside a `results' macro and placed on the same line as
the inline source block.  The macro is stripped upon export.
Multiline and non-scalar RESULTS from inline source blocks are
not allowed.  With optional argument RESULT-PARAMS controls
insertion of results in the Org mode file.  RESULT-PARAMS can
take the following values:

replace - (default option) insert results after the source block
          or inline source block replacing any previously
          inserted results.

silent -- no results are inserted into the Org buffer but
          the results are echoed to the minibuffer and are
          ingested by Emacs (a potentially time consuming
          process).

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org file syntax.

list ---- the results are interpreted as an Org list.

raw ----- results are added directly to the Org file.  This is
          a good option if you code block will output Org
          formatted text.

drawer -- results are added directly to the Org file as with
          \"raw\", but are wrapped in a RESULTS drawer or results
          macro, allowing them to later be replaced or removed
          automatically.

org ----- results are added inside of a \"src_org{}\" or \"#+BEGIN_SRC
          org\" block depending on whether the current source block is
          inline or not.  They are not comma-escaped when inserted,
          but Org syntax here will be discarded when exporting the
          file.

html ---- results are added inside of a #+BEGIN_EXPORT HTML block
          or html export snippet depending on whether the current
          source block is inline or not.  This is a good option
          if your code block will output html formatted text.

latex --- results are added inside of a #+BEGIN_EXPORT LATEX
          block or latex export snippet depending on whether the
          current source block is inline or not.  This is a good
          option if your code block will output latex formatted
          text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a source block with the source-code language
          set appropriately.  Also, source block inlining is
          preserved in this case.  Note this relies on the
          optional LANG argument.

list ---- the results are rendered as a list.  This option not
          allowed for inline source blocks.

table --- the results are rendered as a table.  This option not
          allowed for inline source blocks.

INFO may provide the values of these header arguments (in the
`header-arguments-alist' see the docstring for
`org-babel-get-src-block-info'):

:file --- the name of the file to which output should be written.

:wrap --- the effect is similar to `latex' in RESULT-PARAMS but
          using the argument supplied to specify the export block
          or snippet type."
  (cond ((stringp result)
	 (setq result (org-no-properties result))
	 (when (member "file" result-params)
	   (setq result (org-babel-result-to-file
			 result (when (assq :file-desc (nth 2 info))
				  (or (cdr (assq :file-desc (nth 2 info)))
				      result))))))
	((listp result))
	(t (setq result (format "%S" result))))
  (if (and result-params (member "silent" result-params))
      (progn (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	     result)
    (let ((inline (let ((context (org-element-context)))
		    (and (memq (org-element-type context)
			       '(inline-babel-call inline-src-block))
			 context))))
      (when inline
	(let ((warning
	       (or (and (member "table" result-params) "`:results table'")
		   (and (listp result) "list result")
		   (and (string-match-p "\n." result) "multiline result")
		   (and (member "list" result-params) "`:results list'"))))
	  (when warning
	    (user-error "Inline error: %s cannot be used" warning))))
      (save-excursion
	(let* ((visible-beg (point-min-marker))
	       (visible-end (copy-marker (point-max) t))
	       (inline (let ((context (org-element-context)))
			 (and (memq (org-element-type context)
				    '(inline-babel-call inline-src-block))
			      context)))
	       (existing-result (org-babel-where-is-src-block-result t nil hash))
	       (results-switches (cdr (assq :results_switches (nth 2 info))))
	       ;; When results exist outside of the current visible
	       ;; region of the buffer, be sure to widen buffer to
	       ;; update them.
	       (outside-scope (and existing-result
				   (buffer-narrowed-p)
				   (or (> visible-beg existing-result)
				       (<= visible-end existing-result))))
	       beg end indent)
	  ;; Ensure non-inline results end in a newline.
	  (when (and (org-string-nw-p result)
		     (not inline)
		     (not (string-equal (substring result -1) "\n")))
	    (setq result (concat result "\n")))
	  (unwind-protect
	      (progn
		(when outside-scope (widen))
		(if existing-result (goto-char existing-result)
		  (goto-char (org-element-property :end inline))
		  (skip-chars-backward " \t"))
		(unless inline
		  (setq indent (current-indentation))
		  (forward-line 1))
		(setq beg (point))
		(cond
		 (inline
		   ;; Make sure new results are separated from the
		   ;; source code by one space.
		   (unless existing-result
		     (insert " ")
		     (setq beg (point))))
		 ((member "replace" result-params)
		  (delete-region (point) (org-babel-result-end)))
		 ((member "append" result-params)
		  (goto-char (org-babel-result-end)) (setq beg (point-marker)))
		 ((member "prepend" result-params))) ; already there
		(setq results-switches
		      (if results-switches (concat " " results-switches) ""))
		(let ((wrap
		       (lambda (start finish &optional no-escape no-newlines
				      inline-start inline-finish)
			 (when inline
			   (setq start inline-start)
			   (setq finish inline-finish)
			   (setq no-newlines t))
			 (let ((before-finish (copy-marker end)))
			   (goto-char end)
			   (insert (concat finish (unless no-newlines "\n")))
			   (goto-char beg)
			   (insert (concat start (unless no-newlines "\n")))
			   (goto-char end))))
		      (tabulablep
		       (lambda (r)
			 ;; Non-nil when result R can be turned into
			 ;; a table.
                         (and (proper-list-p r)
			      (cl-every
                               (lambda (e) (or (atom e) (proper-list-p e)))
			       result)))))
		  ;; insert results based on type
		  (cond
		   ;; Do nothing for an empty result.
		   ((null result))
		   ;; Insert a list if preferred.
		   ((member "list" result-params)
		    (insert
		     (org-trim
		      (org-list-to-generic
		       (cons 'unordered
			     (mapcar
			      (lambda (e)
				(list (if (stringp e) e (format "%S" e))))
			      (if (listp result) result
				(split-string result "\n" t))))
		       '(:splicep nil :istart "- " :iend "\n")))
		     "\n"))
		   ;; Try hard to print RESULT as a table.  Give up if
		   ;; it contains an improper list.
		   ((funcall tabulablep result)
		    (goto-char beg)
		    (insert (concat (orgtbl-to-orgtbl
				     (if (cl-every
					  (lambda (e)
					    (or (eq e 'hline) (listp e)))
					  result)
					 result
				       (list result))
				     nil)
				    "\n"))
		    (goto-char beg)
		    (when (org-at-table-p) (org-table-align))
		    (goto-char (org-table-end)))
		   ;; Print verbatim a list that cannot be turned into
		   ;; a table.
		   ((listp result) (insert (format "%s\n" result)))
		   ((member "file" result-params)
		    (when inline
		      (setq result (org-macro-escape-arguments result)))
		    (insert result))
		   ((and inline (not (member "raw" result-params)))
		    (insert (org-macro-escape-arguments
			     (org-babel-chomp result "\n"))))
		   (t (goto-char beg) (insert result)))
		  (setq end (copy-marker (point) t))
		  ;; possibly wrap result
		  (cond
		   ((assq :wrap (nth 2 info))
		    (let ((name (or (cdr (assq :wrap (nth 2 info))) "results")))
		      (funcall wrap (concat "#+begin_" name)
			       (concat "#+end_" (car (split-string name)))
			       nil nil (concat "{{{results(@@" name ":") "@@)}}}")))
		   ((member "html" result-params)
		    (funcall wrap "#+begin_export html" "#+end_export" nil nil
			     "{{{results(@@html:" "@@)}}}"))
		   ((member "latex" result-params)
		    (funcall wrap "#+begin_export latex" "#+end_export" nil nil
			     "{{{results(@@latex:" "@@)}}}"))
		   ((member "org" result-params)
		    (goto-char beg) (when (org-at-table-p) (org-cycle))
		    (funcall wrap "#+begin_src org" "#+end_src" nil nil
			     "{{{results(src_org{" "})}}}"))
		   ((member "code" result-params)
		    (let ((lang (or lang "none")))
		      (funcall wrap (format "#+begin_src %s%s" lang results-switches)
			       "#+end_src" nil nil
			       (format "{{{results(src_%s[%s]{" lang results-switches)
			       "})}}}")))
		   ((member "raw" result-params)
		    (goto-char beg) (when (org-at-table-p) (org-cycle)))
		   ((or (member "drawer" result-params)
			;; Stay backward compatible with <7.9.2
			(member "wrap" result-params))
		    (goto-char beg) (when (org-at-table-p) (org-cycle))
		    (funcall wrap ":results:" ":end:" 'no-escape nil
			     "{{{results(" ")}}}"))
		   ((and inline (member "file" result-params))
		    (funcall wrap nil nil nil nil "{{{results(" ")}}}"))
		   ((and (not (funcall tabulablep result))
			 (not (member "file" result-params)))
		    (let ((org-babel-inline-result-wrap
			   ;; Hard code {{{results(...)}}} on top of
			   ;; customization.
			   (format "{{{results(%s)}}}"
				   org-babel-inline-result-wrap)))
		      (org-babel-examplify-region
		       beg end results-switches inline)))))
		;; Possibly indent results in par with #+results line.
		(when (and (not inline) (numberp indent) (> indent 0)
			   ;; In this case `table-align' does the work
			   ;; for us.
			   (not (and (listp result)
				     (member "append" result-params))))
		  (indent-rigidly beg end indent))
		(if (null result)
		    (if (member "value" result-params)
			(message "Code block returned no value.")
		      (message "Code block produced no output."))
		  (message "Code block evaluation complete.")))
	    (set-marker end nil)
	    (when outside-scope (narrow-to-region visible-beg visible-end))
	    (set-marker visible-beg nil)
	    (set-marker visible-end nil)))))))

(advice-add 'org-babel-insert-result :override #'mh/org-babel-insert-result)

(org-babel-lob-ingest (concat user-emacs-directory "layers/org/lob.org"))

(load (concat user-emacs-directory "layers/org/babel-latex.el"))

;;; babel.el ends here
