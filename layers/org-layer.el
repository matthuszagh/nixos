;;; org-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org
  :presetup
  (:layer straight
   (straight-use-package 'polymode)
   (straight-use-package 'poly-org))
  (:layer (straight helm)
   (straight-use-package 'helm-org-rifle))
  (:layer modal
   (general-def 'normal 'motion
     "<tab>" nil))

  :setup
  (use-package org
    :demand t
    :hook
    ((org-mode . (lambda ()
                   (setq-local fill-column 70)))
     (org-babel-after-execute . org-display-inline-images)
     (org-mode . (lambda ()
                   (company-mode -1))))
    :config
    (defun mh//override-org-create-formula-image
        (string tofile options buffer &optional processing-type)
      "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
      (let* ((processing-type (or processing-type
			          org-preview-latex-default-process))
	     (processing-info
	      (cdr (assq processing-type org-preview-latex-process-alist)))
	     (programs (plist-get processing-info :programs))
	     (error-message (or (plist-get processing-info :message) ""))
	     (image-input-type (plist-get processing-info :image-input-type))
	     (image-output-type (plist-get processing-info :image-output-type))
	     (post-clean (or (plist-get processing-info :post-clean)
			     '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
			       ".svg" ".png" ".jpg" ".jpeg" ".out")))
	     (latex-header
	      (or (plist-get processing-info :latex-header)
	          (org-latex-make-preamble
	           (org-export-get-environment (org-export-get-backend 'latex))
	           org-format-latex-header
	           'snippet)))
	     (latex-compiler (plist-get processing-info :latex-compiler))
	     (image-converter (plist-get processing-info :image-converter))
	     (tmpdir temporary-file-directory)
	     (texfilebase (make-temp-name
		           (expand-file-name "orgtex" tmpdir)))
	     (texfile (concat texfilebase ".tex"))
	     (image-size-adjust (or (plist-get processing-info :image-size-adjust)
				    '(1.0 . 1.0)))
	     (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
		       (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
	     (dpi (* scale (if buffer (org--get-display-dpi) 140.0)))
	     (fg (or (plist-get options (if buffer :foreground :html-foreground))
		     "Black"))
	     (bg (or (plist-get options (if buffer :background :html-background))
		     "Transparent"))
	     (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
	     (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
        (dolist (program programs)
          (org-check-external-command program error-message))
        (if (eq fg 'default)
	    (setq fg (org-latex-color :foreground))
          (setq fg (org-latex-color-format fg)))
        (if (eq bg 'default)
	    (setq bg (org-latex-color :background))
          (setq bg (org-latex-color-format
		    (if (string= bg "Transparent") "white" bg))))
        ;; remove tex \par at end of snippet to avoid trailing
        ;; whitespace
        (if (string= (substring string -1 nil) "\n")
            (aset string (- (length string) 1) ?%)
          (setq string (concat string "%")))
        (with-temp-file texfile
          (insert latex-header)
          (insert "\n\\begin{document}\n"
	          "\\definecolor{fg}{rgb}{" fg "}%\n"
	          "\\definecolor{bg}{rgb}{" bg "}%\n"
	          "\n\\pagecolor{bg}%\n"
	          "\n{\\color{fg}\n"
	          string
	          "\n}\n"
	          "\n\\end{document}\n"))
        (let* ((err-msg (format "Please adjust `%s' part of \
`org-preview-latex-process-alist'."
			        processing-type))
	       (image-input-file
	        (org-compile-file
	         texfile latex-compiler image-input-type err-msg log-buf))
	       (image-output-file
	        (org-compile-file
	         image-input-file image-converter image-output-type err-msg log-buf
	         `((?D . ,(shell-quote-argument (format "%s" dpi)))
	           (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
          (copy-file image-output-file tofile 'replace)
          (dolist (e post-clean)
	    (when (file-exists-p (concat texfilebase e))
	      (delete-file (concat texfilebase e))))
          image-output-file)))

    (advice-add 'org-create-formula-image :override #'mh//override-org-create-formula-image)

    (use-package org-element
      :config
      ;; permit the use of latex environments in inline math delimiters
      (setq org-element--latex-begin-environment "^[ \t]*\\(?:\\\\(\\|\\$\\)?\\\\begin{\\([A-Za-z0-9*]+\\)}")
      (setq org-element--latex-end-environment "\\\\end{%s}[ \t]*\\(?:\\\\)\\|\\$\\)?$"))

    (general-define-key
     :keymaps 'org-mode-map
     "TAB" 'org-cycle
     "C-RET" 'org-insert-heading-respect-content)

    (setq org-startup-with-latex-preview t)
    (setq org-startup-with-inline-images t)

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    (setq org-enforce-todo-dependencies t)

    ;; always use :noweb in org babel source blocks.
    (setq org-babel-default-header-args
          (cons '(:noweb . "yes")
                (assq-delete-all :noweb org-babel-default-header-args)))

    ;; use habits
    (add-to-list 'org-modules 'org-habit)
    (setq org-habit-show-habits nil)

    ;; property inheritance
    (setq org-use-property-inheritance t)

    ;; agenda view
    (setq org-agenda-custom-commands
          '(("c" "Custom agenda view"
             ((tags-todo "work"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Work")))
              (agenda "" ((org-agenda-span 'day)))
              (tags-todo "+nix+PRIORITY=\"A\""
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Nix")))
              (tags-todo "emacs"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Emacs")))
              (tags-todo "read"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Read")))
              (tags-todo "physics"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Physics")))
              (tags-todo "electronics"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Electronics")))))))

    ;; todo statistics should display all recursive children
    (setq org-hierarchical-todo-statistics nil)
    ;; set default image background color
    (defun org-display-inline-images--with-color-theme-background-color (args)
      "Specify background color of Org-mode inline image through modify `ARGS'."
      (let* ((file (car args))
             (type (cadr args))
             (data-p (caddr args))
             (props (cdddr args)))
        ;; get this return result style from `create-image'
        (append (list file type data-p)
                (list :background "white")
                props)))

    (advice-add 'create-image :filter-args
                #'org-display-inline-images--with-color-theme-background-color)

    ;; use actual width for inline image
    (setq org-image-actual-width t)

    ;; fontify when not in polymode
    (setq org-src-fontify-natively t)

    ;; preserve src block indentation
    ;; this works better with aggressive-indent-mode
    (setq org-edit-src-content-indentation 0)
    (setq org-src-preserve-indentation t)

    ;; don't display emphasis characters
    (setq org-hide-emphasis-markers t)

    ;; don't trigger error for broken links during export
    (setq org-export-with-broken-links t)

    ;; permit still typing emphasis characters as normal characters
    ;; see https://emacs.stackexchange.com/a/16746/20317
    (defun mh/org-entity-get-name (char)
      "Return the entity name for CHAR. For example, return \"ast\" for *."
      (let ((ll (append org-entities-user
                        org-entities))
            e name utf8)
        (catch 'break
          (while ll
            (setq e (pop ll))
            (when (not (stringp e))
              (setq utf8 (nth 6 e))
              (when (string= char utf8)
                (setq name (car e))
                (throw 'break name)))))))

    (defun mh/org-insert-org-entity-maybe (&rest args)
      "When the universal prefix C-u is used before entering any character,
  insert the character's `org-entity' name if available.

  If C-u prefix is not used and if `org-entity' name is not available, the
  returned value `entity-name' will be nil."
      ;; It would be fine to use just (this-command-keys) instead of
      ;; (substring (this-command-keys) -1) below in emacs 25+.
      ;; But if the user pressed "C-u *", then
      ;;  - in emacs 24.5, (this-command-keys) would return "^U*", and
      ;;  - in emacs 25.x, (this-command-keys) would return "*".
      ;; But in both versions, (substring (this-command-keys) -1) will return
      ;; "*", which is what we want.
      ;; http://thread.gmane.org/gmane.emacs.orgmode/106974/focus=106996
      (let ((pressed-key (substring (this-command-keys) -1))
            entity-name)
        (when (and (listp args) (eq 4 (car args)))
          (setq entity-name (mh/org-entity-get-name pressed-key))
          (when entity-name
            (setq entity-name (concat "\\" entity-name "{}"))
            (insert entity-name)
            (message (concat "Inserted `org-entity' "
                             (propertize entity-name
                                         'face 'font-lock-function-name-face)
                             " for the symbol "
                             (propertize pressed-key
                                         'face 'font-lock-function-name-face)
                             "."))))
        entity-name))

    ;; Run `org-self-insert-command' only if `mh/org-insert-org-entity-maybe'
    ;; returns nil.
    (advice-add 'org-self-insert-command :before-until #'mh/org-insert-org-entity-maybe)
    (setq org-pretty-entities t)

    ;; `org-adapt-indentation' indents heading contents to the beginning of the heading. This is nice
    ;; in a way, but limits the amount of horizontal space when you have deeply-nested headings.
    (setq org-adapt-indentation nil)
    (setq org-log-done 'time
          org-todo-keyword-faces '(("INPROGRESS" . (:foreground "#cc8800"))))
    (setq org-todo-keywords
          '((sequence "HOLD" "TODO" "|" "DONE" "CANCELLED")))
    ;; (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
    (setq org-capture-templates
          '(("b" "pdf" entry (file "/home/matt/doc/notes/wiki.org")
             "* %f
:PROPERTIES:
:Filepath: %a
:END:
** description
** outline [/]
%(mh/pdf-outline-to-org-headline \"%F\" 2 nil)")
            ("p" "productivity" entry (file+headline "/home/matt/doc/notes/projects/productivity.org" "refile")
             "* TODO %^{PROMPT}")
            ("w" "work" entry (file+headline "/home/matt/doc/notes/projects/work.org" "refile")
             "* TODO %^{PROMPT}")))
    (setq org-agenda-files (append
                            '("/home/matt/src/dotfiles/README.org")
                            (directory-files-recursively "/home/matt/doc/notes" "org$")))
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 10))))
    (setq org-refile-use-outline-path t)
    (setq org-agenda-follow-mode t)
    ;; include plain lists in org cycling, which folds lists by default when a heading is first
    ;; expanded.
    (setq org-cycle-include-plain-lists 'integrate)
    ;; Set file for the current entry.
    (defun org-set-property-file (file)
      (interactive
       (list
        (read-file-name "file: " "/home/matt/library/")))
      (org-set-property "Filepath" (concat "[[file:" file "]]")))
    ;; Outline percentage completion includes all children of node rather than just the direct
    ;; children.
    (setq org-checkbox-hierarchical-statistics nil)

    ;; this uses crop instead of preview for standalone. In this mode
    ;; math must be inlined, which is annoying, but it does produce
    ;; correctly sized image outputs. Preview crops the images for
    ;; some reason.
    (setq org-format-latex-header "% Needed for proper rendering with some corner cases in luatex
\\RequirePackage{luatex85}
\\PassOptionsToPackage{usenames}{xcolor}
\\documentclass[border={0pt 1pt}]{standalone}
\[PACKAGES]
\[DEFAULT-PACKAGES]

%% color definitions
\\definecolor{bgcolor}{rgb}{0.133333333333, 0.133333333333, 0.133333333333}
\\definecolor{fgcolor}{rgb}{0.764705882353, 0.764705882353, 0.690196078431}

%% Circuitikz style options
\\ctikzset{-o/.style = {bipole nodes={none}{ocirc, fill=bgcolor}}}
\\ctikzset{o-/.style = {bipole nodes={ocirc, fill=bgcolor}{none}}}
\\ctikzset{o-o/.style = {bipole nodes={ocirc, fill=bgcolor}{ocirc, fill=bgcolor}}}
\\ctikzset{*-o/.style = {bipole nodes={circ}{ocirc, fill=bgcolor}}}
\\ctikzset{o-*/.style = {bipole nodes={ocirc, fill=bgcolor}{circ}}}
\\ctikzset{resistors/scale=0.6, capacitors/scale=0.6, diodes/scale=0.4}

%% tikz libraries
\\usetikzlibrary{intersections}
\\usetikzlibrary{3d}
\\usetikzlibrary{perspective}
\\usetikzlibrary{shapes.geometric}
\\usetikzlibrary{decorations.markings}
\\usetikzlibrary{positioning}
\\usetikzlibrary{automata}
\\usetikzlibrary{patterns}

%% tikz settings
\\tikzset{>=latex}
% Important equation emphasis box
\\def\\eqnBoxCol{gray!30!bgcolor}
\\tikzset{emphBox/.style={draw=\\eqnBoxCol, fill=\\eqnBoxCol, thick, rectangle, inner sep=5pt, inner ysep=10pt}}

%% pgfplots
\\pgfplotsset{compat=newest}
\\pgfplotsset{
  every non boxed x axis/.style={
    xtick align=center,
    enlarge x limits=upper,
    x axis line style={-latex},
  },
  every boxed x axis/.style={},
  every non boxed y axis/.style={
    ytick align=center,
    enlarge y limits=upper,
    y axis line style={-latex},
  },
  every boxed y axis/.style={},
}
\\usepgfplotslibrary{groupplots}

%% tikztiminglibraries
\\usetikztiminglibrary{counters}")

    ;; change default latex packages. grffile prevents asymptote from
    ;; working correctly. inputenc and fontenc aren't needed with
    ;; luatex.
    (setq org-latex-default-packages-alist
          '(("" "graphicx" t)
            ("" "longtable" nil)
            ("" "wrapfig" nil)
            ("" "rotating" nil)
            ("normalem" "ulem" t)
            ("" "amsmath" t)
            ("" "textcomp" t)
            ("" "amssymb" t)
            ("" "capt-of" nil)
            ("" "hyperref" nil)))

    ;; (setq org-latex-default-class "standalone")
    (add-to-list 'org-latex-packages-alist
                 '("" "tikz" t))
    (add-to-list 'org-latex-packages-alist
                 '("" "pgfplots" t))
    (add-to-list 'org-latex-packages-alist
                 '("" "circuitikz" t))
    (add-to-list 'org-latex-packages-alist
                 '("" "tikz-timing" t))
    (add-to-list 'org-latex-packages-alist
                 '("" "mathtools" t))
    (add-to-list 'org-latex-packages-alist
                 '("" "siunitx" t))
    (add-to-list 'org-latex-packages-alist
                 '("" "bm" t))
    (add-to-list 'org-latex-packages-alist
                 '("" "tabularx" t))
    ;; needed for multicolumns in tables
    (add-to-list 'org-latex-packages-alist
                 '("" "booktabs" t))
    (add-to-list 'org-latex-packages-alist
                 '("inline" "asymptote" t))
    (setq org-latex-create-formula-image-program 'imagemagick)
    ;; necessary for drawing electronics circuits with tikz using the `circuits' library.
    ;; (setq org-format-latex-header
    ;;       (concat org-format-latex-header
    ;;               "\\usetikzlibrary{circuits.logic.US,circuits.logic.IEC,circuits.ee.IEC}"))

    ;; make clocking efforts persistant across emacs sessions.
    ;; see [[info:org#Clocking%20Work%20Time][info:org#Clocking Work Time]]
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

    ;; always leave a newline at the end of a heading section. `auto'
    ;; doesn't seem to be good enough at guessing.
    (setq org-blank-before-new-entry
          '((heading . t)
            (plain-list-item . auto)))

    ;; set the column view format to include effort
    (setq org-columns-default-format (concat "%60ITEM(Task) "
                                             ;; "%TODO %3PRIORITY "
                                             "%17Effort(Estimated Effort){:} "
                                             "%CLOCKSUM"))
    ;; keep the same column format in the agenda columns view
    (setq org-agenda-overriding-columns-format org-columns-default-format)

    ;; (plist-put org-format-latex-options :background "white")

    ;; (setq org-latex-pdf-process
    ;;       (list "sed -i 's/\\begin{latex}//'"
    ;;             "sed -i 's/\\end{latex}//'"
    ;;             "latexmk -f -pdf %F"))

    ;; ;; lualatex preview
    (setq org-latex-pdf-process
          '("latexmk -f -interaction=nonstopmode -output-directory=%o %f"))

    (setq luamagick
          '(luamagick :programs ("latexmk" "lualatex" "convert")
                      :description "pdf > png"
                      :message "you need to install latexmk, lualatex and imagemagick."
                      :use-xcolor t
                      :image-input-type "pdf"
                      :image-output-type "png"
                      :image-size-adjust (1.0 . 1.0)
                      :latex-compiler ("latexmk -cd -f -diagnostics -interaction=nonstopmode -output-directory=%o %f")
                      :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

    (setq luasvgm
          '(luasvgm :programs ("latexmk" "lualatex" "dvisvgm")
                    :description "pdf > svg"
                    :message "you need to install latexmk, lualatex and dvisvgm."
                    :use-xcolor t
                    :image-input-type "pdf"
                    :image-output-type "svg"
                    :image-size-adjust (2.0 . 1.0)
                    :latex-compiler ("latexmk -f -interaction=nonstopmode -output-directory=%o %f")
                    :image-converter ("dvisvgm %f -P -n -b min -c %S -o %O")))

    (add-to-list 'org-preview-latex-process-alist luamagick)
    (add-to-list 'org-preview-latex-process-alist luasvgm)

    ;; TODO this requires dvisvgm >= 2.7.2, which requires overriding
    ;; the version texlive uses.
    ;; (setq org-preview-latex-default-process 'luasvgm)
    (setq org-preview-latex-default-process 'luamagick)

    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (asymptote . t)
       (awk . t)
       (calc . t)
       (clojure . t)
       (comint . t)
       (css . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (fortran . t)
       (gnuplot . t)
       (haskell . t)
       (io . t)
       (java . t)
       (js . t)
       (latex . t)
       (ledger . t)
       (lilypond . t)
       (lisp . t)
       (lua . t)
       (makefile . t)
       (matlab . t)
       (maxima . t)
       (mscgen . t)
       (ocaml . t)
       (octave . t)
       (org . t)
       (perl . t)
       (picolisp . t)
       (plantuml . t)
       (python . t)
       (ref . t)
       (ruby . t)
       (sass . t)
       (scheme . t)
       (screen . t)
       (shell . t)
       (shen . t)
       (sql . t)
       (sqlite . t)))

    ;; org crypt
    (use-package org-crypt
      :config
      (setq org-crypt-disable-auto-save t)
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
      (setq org-crypt-key "huszaghmatt@gmail.com"))

    ;; identify org headlines with UUIDs
    ;; see https://writequit.org/articles/emacs-org-mode-generate-ids.html
    (require 'org-id)
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

    ;; make contrib files visible
    ;; TODO modify this for nixpkgs
    (add-to-list 'load-path "~/.emacs.d/straight/repos/org/contrib/lisp" t))

  (use-package org-drill
    :config
    (setq org-drill-hide-item-headings-p t))

  ;; (use-package polymode)

  ;; (use-package poly-org
  ;;   :after (polymode org)
  ;;   :config
  ;;   (add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode)))

  (use-package org-edna
    :config
    (org-edna-load))

  (use-package ob-sagemath
    :config
    (setq org-babel-default-header-args:sage '((:session . t)
                                               (:results . "output"))))

  ;; use org-board for web archival
  (use-package org-board
    ;; :config
    ;; (setq org-board-wget-switches '("-e robots=off"
    ;;                                 "--page-requisites"
    ;;                                 "--adjust-extension"
    ;;                                 "--convert-links"
    ;;                                 "--recursive"
    ;;                                 "-l 1"
    ;;                                 "--span-hosts"))
    )

  :postsetup
  (:layer pdf
   (defun mh/open-book-from-outline ()
     (interactive)
     (setq file (car (s-split "]" (car (last (s-split "file:" (org-entry-get (point) "Filepath" t)))))))
     (setq page (string-to-number (car (s-match "[0-9]+"
                                                (car (s-match "([0-9]+)" (org-entry-get nil "ITEM")))))))
     (find-file-other-window file)
     (pdf-view-goto-page page))

   (defun mh/pdf-outline-to-org-headline (file base-depth todop)
     "Return a set of org headings from a pdf in FILE.
BASE-DEPTH is the depth (i.e. number of '*') of the destination
org file headline, and TODOP asks whether we should turn the
outline into a set of todo entries. Set 1 for yes and 0 for no.

Do not call this directly! It will simply discard the result. Use
org-capture instead."
     (interactive
      "fPDF file: \nnHeadline Depth: \nSTODO Headline? (t / nil): ")
     (if todop
         (let ((outline (pdf-info-outline file))
               (org-outline "")
               (first-headline t)
               (last-item '()))
           (dolist (item outline)
             (if last-item
                 (let-alist last-item
                   (setq i (+ .depth base-depth))
                   (while (> i 0)
                     (setq org-outline (concat org-outline "*"))
                     (setq i (- i 1)))
                   (if first-headline
                       (setq org-outline (concat org-outline " TODO"))
                     (setq org-outline (concat org-outline " HOLD")))
                   (setq org-outline (concat org-outline " " .title))
                   (setq org-outline (concat org-outline " (" (number-to-string .page) ")\n"))
                   (if (not (> (alist-get 'depth item) .depth))
                       (setq org-outline (concat org-outline ":PROPERTIES:\n\
:TRIGGER: next-sibling todo!(TODO)\n\
:BLOCKER: previous-sibling\n\
:END:\n")))
                   (if (and first-headline
                            (not (> (alist-get 'depth item) .depth)))
                       (setq first-headline nil))))
             (setq last-item item))
           (let-alist (car (-take-last 1 outline))
             (setq i (+ .depth base-depth))
             (while (> i 0)
               (setq org-outline (concat org-outline "*"))
               (setq i (- i 1)))
             (if first-headline
                 (setq org-outline (concat org-outline " TODO"))
               (setq org-outline (concat org-outline " HOLD")))
             (setq org-outline (concat org-outline " " .title))
             (setq org-outline (concat org-outline " (" (number-to-string .page) ")\n"))
             (setq org-outline (concat org-outline ":PROPERTIES:\n\
:TRIGGER: next-sibling todo!(TODO)\n\
:BLOCKER: previous-sibling\n\
:END:\n")))
           org-outline)
       (let ((outline (pdf-info-outline file))
             (org-outline ""))
         (dolist (item outline)
           (let-alist item
             (setq i (+ .depth base-depth))
             (while (> i 0)
               (setq org-outline (concat org-outline "*"))
               (setq i (- i 1)))
             (setq org-outline (concat org-outline " " .title))
             (setq org-outline (concat org-outline " (" (number-to-string .page) ")\n"))))
         org-outline))))

  (:layer (pdf modal)
   (localleader :keymaps 'org-mode-map
     "b" 'mh/open-book-from-outline))

  (:layer helm
   (use-package helm-org-rifle
     :config
     (setq helm-org-rifle-show-path t)
     (setq helm-org-rifle-test-against-path t))
   (setq helm-org-headings-max-depth 50))

  (:layer (helm modal)
   (general-def mh/prefix-search-map
     "h" 'helm-org-in-buffer-headings))

  (:layer modal
   ;; evil appears to override certain org-mode keybindings with the
   ;; outline-mode counterparts. revert them here.
   (general-define-key
    :keymaps 'org-mode-map
    :states '(normal motion)
    "<tab>" 'org-cycle
    "g p" 'org-mark-ring-goto
    "g d" 'org-open-at-point)

   (defun mh/tex-insert-frac ()
     "Insert '\frac{}{}' and position point before the first right brace."
     (interactive)
     (insert "\\frac{}{}")
     (backward-char)
     (backward-char)
     (backward-char))

   (defun mh/tex-insert-text-text ()
     "Insert '\text{}' and position point inside the brace."
     (interactive)
     (insert "\\text{}")
     (backward-char))

   (defun mh/tex-insert-text-bf ()
     "Insert '\textbf{}' and position point inside the brace."
     (interactive)
     (insert "\\textbf{}")
     (backward-char))

   (defun mh/tex-insert-text-it ()
     "Insert '\textit{}' and position point inside the brace."
     (interactive)
     (insert "\\textit{}")
     (backward-char))

   (defun mh/tex-insert-text-tt ()
     "Insert '\texttt{}' and position point inside the brace."
     (interactive)
     (insert "\\texttt{}")
     (backward-char))

   (defun mh/tex-insert-math-subscript ()
     "Insert '_{\text{}}' and cursor to point inside middle brace."
     (interactive)
     (insert "_{\\text{}}")
     (backward-char)
     (backward-char))

   (defun mh/tex-insert-left-delimiter ()
     "Insert '\left('."
     (interactive)
     (insert "\\left("))

   (defun mh/tex-insert-right-delimiter ()
     "Insert '\right)'."
     (interactive)
     (insert "\\right)"))

   (defun mh/tex-insert-sine ()
     (interactive)
     (insert "\\sin\\left(\\right)")
     (backward-char 7))

   (defun mh/tex-insert-cosine ()
     (interactive)
     (insert "\\cos\\left(\\right)")
     (backward-char 7))

   (defun mh/tex-insert-tangent ()
     (interactive)
     (insert "\\tan\\left(\\right)")
     (backward-char 7))

   (defun mh/tex-insert-enviro-align ()
     (interactive)
     (insert "\\(\\begin{aligned}\n")
     (insert "  \n")
     (insert "\\end{aligned}\\)")
     (previous-line)
     (move-end-of-line nil))

   (defun mh/tex-insert-enviro-equation ()
     (interactive)
     (insert "\\(\\displaystyle\n")
     (insert "  \n")
     (insert "\\)")
     (previous-line)
     (move-end-of-line nil))

   (defun mh/tex-insert-enviro-tikz ()
     (interactive)
     (insert "\\begin{tikzpicture}\n")
     (insert "  \n")
     (insert "\\end{tikzpicture}")
     (previous-line)
     (move-end-of-line nil))

   (defun mh/tex-insert-enviro-circuitikz ()
     (interactive)
     (insert "\\begin{circuitikz}\n")
     (insert "  \n")
     (insert "\\end{circuitikz}")
     (previous-line)
     (move-end-of-line nil))

   (defun mh/tex-insert-enviro-latex ()
     (interactive)
     (insert "\\begin{latex}\n")
     (insert "  \n")
     (insert "\\end{latex}")
     (previous-line)
     (move-end-of-line nil))

   (defun mh/tex-insert-subscript (arg)
     (interactive "Msubscript: ")
     (insert (concat "_{" arg "}")))

   (defun mh/tex-insert-superscript (arg)
     (interactive "Msuperscript: ")
     (insert (concat "^{" arg "}")))

   ;; latex insert shortcuts
   (general-define-key
    :prefix-command 'mh/command-org-tex-insert-enviro-prefix
    "a" 'mh/tex-insert-enviro-align
    "e" 'mh/tex-insert-enviro-equation
    "t" 'mh/tex-insert-enviro-tikz
    "c" 'mh/tex-insert-enviro-circuitikz
    "l" 'mh/tex-insert-enviro-latex)

   (general-define-key
    :prefix-command 'mh/command-org-tex-insert-text-prefix
    "t" 'mh/tex-insert-text-text
    "b" 'mh/tex-insert-text-bf
    "i" 'mh/tex-insert-text-it
    "p" 'mh/tex-insert-text-tt)

   (general-define-key
    :prefix-command 'mh/command-org-tex-insert-prefix
    "f" 'mh/tex-insert-frac
    "t" 'mh/command-org-tex-insert-text-prefix
    "l" 'mh/tex-insert-left-delimiter
    "r" 'mh/tex-insert-right-delimiter
    "s" 'mh/tex-insert-sine
    "c" 'mh/tex-insert-cosine
    "e" 'mh/command-org-tex-insert-enviro-prefix
    "_" 'mh/tex-insert-subscript
    "^" 'mh/tex-insert-superscript)

   (general-def 'normal org-mode-map
     "M-j" 'mc/mark-next-like-this
     "M-k" 'mc/mark-previous-like-this
     "g j" 'org-forward-heading-same-level
     "g k" 'org-backward-heading-same-level
     "g l" 'org-next-visible-heading
     "g h" 'org-previous-visible-heading
     "C-j" 'outline-move-subtree-down
     "C-k" 'outline-move-subtree-up)

   ;; local org mode commands
   (localleader :keymaps 'org-mode-map
     "T" 'org-babel-tangle
     "l" 'org-insert-link
     ":" 'org-set-tags-command
     "i" 'org-id-get-create
     "C" 'org-columns
     "E" 'org-set-effort
     "I" 'org-clock-in
     "a" 'org-archive-subtree
     "s" 'org-insert-structure-template
     "p" 'org-set-property
     "t" 'org-todo
     "r" 'org-refile
     "e" 'org-edit-special
     "c" 'org-ctrl-c-ctrl-c
     "P" 'org-priority
     "L" 'org-toggle-latex-fragment
     "z" 'mh/command-org-tex-insert-prefix
     "o" 'org-open-at-point
     "d" 'define-word-at-point)

   ;; org agenda keys
   ;; override org agenda keys and add back the ones you want
   ;; (setq org-agenda-mode-map (make-composed-keymap general-override-mode-map))
   (evil-set-initial-state 'org-agenda-mode 'normal)
   (general-def 'normal org-agenda-mode-map
     "j" 'org-agenda-next-line
     "k" 'org-agenda-previous-line)

   (localleader :keymaps 'org-agenda-mode-map
     "c" 'org-agenda-columns
     "q" 'org-agenda-quit
     "i" 'org-agenda-clock-in
     "o" 'org-agenda-clock-out
     "t" 'org-agenda-todo
     "e" 'org-agenda-set-effort
     "r" 'org-agenda-redo)

   ;; global org keys
   (general-define-key
    :keymaps 'mh/prefix-map
    :prefix "o"
    :prefix-command 'mh/command-org-prefix
    :prefix-map 'mh/prefix-org-map
    "c" 'org-capture
    "a" 'org-agenda
    "l" 'org-store-link
    "o" 'org-clock-out
    "i" 'org-clock-in-last))

  (:layer internet
   (use-package org-eww))

  :func
  (setq mh-latex-scale 1.0)
  (defun mh/increase-latex-scale ()
    (interactive)
    (setq mh-latex-scale (+ mh-latex-scale 0.1))
    (call-interactively 'revert-buffer))

  (defun mh/decrease-latex-scale ()
    (interactive)
    (setq mh-latex-scale (- mh-latex-scale 0.1))
    (call-interactively 'revert-buffer)))

;;; org-layer.el ends here
