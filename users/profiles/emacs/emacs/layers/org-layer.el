;;; org-layer.el --- Org Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def org
  :depends (tex)

  :presetup
  (:layer straight
   (straight-use-package '(org-plus-contrib
                           :host github
                           :repo "matthuszagh/org-mode"
                           :branch "beta-2"
                           :files ("lisp/*.el" "contrib/lisp/*.el")
        		   :includes (org)
                           :local-repo "org"))
   ;; (straight-use-package 'org-drill)
   (straight-use-package '(org-ml
                           :host github
                           :repo "ndwarshuis/org-ml"))
   (straight-use-package '(org-texnum
                           :host github
                           :repo "matthuszagh/org-texnum"))
   (straight-use-package '(org-api
                           :host github
                           :repo "matthuszagh/org-api"))
   (straight-use-package 'org-fragtog)
   (straight-use-package 'org-edna)
   (straight-use-package 'ob-sagemath)
   (straight-use-package '(ob-spice
                           :repo "https://git.sr.ht/~bzg/org-contrib"))
   (straight-use-package 'org-board)
   (straight-use-package 'htmlize)
   (straight-use-package 's))
  (:layer (straight helm)
   (straight-use-package 'helm-org-rifle))
  (:layer modal
   (general-def 'normal 'motion
     "<tab>" nil))

  :setup
  (use-package org
    :demand t
    :hook
    ((org-babel-after-execute . org-display-inline-images))
    :config
    (use-package org-element)

    (setq org-startup-with-latex-preview t)
    (setq org-startup-with-inline-images t)
    (setq org-startup-folded t)

    ;; show invisible text when editing it
    (setq org-catch-invisible-edits 'show)

    ;; place archives in the current file under the top-level 'archive' headline
    (setq org-archive-location "::* archive")

    ;; hide emphasis markers
    (setq org-hide-emphasis-markers t)

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


    ;; allow the use of :hidden to hide certain source blocks when a
    ;; buffer is opened. All others will be visible by default.
    (defun mh//individual-visibility-source-blocks ()
      "Fold some blocks in the current buffer."
      (interactive)
      (org-show-block-all)
      (org-block-map
       (lambda ()
         (let ((case-fold-search t))
           (when (and
                  (save-excursion
                    (beginning-of-line 1)
                    (looking-at org-block-regexp))
                  (cl-assoc
                   ':hidden
                   (cl-third
                    (org-babel-get-src-block-info))))
             (org-hide-block-toggle))))))
    (add-hook 'org-mode-hook (function mh//individual-visibility-source-blocks))

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
              ;; (agenda "" ((org-agenda-span 'day)))
              (tags-todo "nix"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Nix")))
              (tags-todo "emacs"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Emacs")))
              (tags-todo "software"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Software")))
              (tags-todo "hardware"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Hardware")))
              (tags-todo "circuits"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Circuits")))
              (tags-todo "read"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("HOLD")))
                          (org-agenda-overriding-header "Prioritized Reading Material")))))))

    ;; todo statistics should display all recursive children
    (setq org-hierarchical-todo-statistics nil)
    ;; ;; set default image background color
    ;; (defun org-display-inline-images--with-color-theme-background-color (args)
    ;;   "Specify background color of Org-mode inline image through modify `ARGS'."
    ;;   (let* ((file (car args))
    ;;          (type (cadr args))
    ;;          (data-p (caddr args))
    ;;          (props (cdddr args)))
    ;;     ;; get this return result style from `create-image'
    ;;     (append (list file type data-p)
    ;;             (list :background "white")
    ;;             props)))

    ;; (advice-add 'create-image :filter-args
    ;;             #'org-display-inline-images--with-color-theme-background-color)
    ;; (advice-remove 'create-image #'org-display-inline-images--with-color-theme-background-color)

    ;; look for a specified attribute width, otherwise fallback to
    ;; actual image width
    (setq org-image-actual-width nil)

    ;; fontify when not in polymode
    (setq org-src-fontify-natively t)

    ;; preserve src block indentation
    ;; this works better with aggressive-indent-mode
    (setq org-edit-src-content-indentation 0)
    (setq org-src-preserve-indentation t)

    ;; Don't pretty display things like pi. This makes it harder to
    ;; edit latex code.
    (setq org-pretty-entities nil)
    ;; When displaying pretty entities, don't display
    ;; super/subscripts.
    (setq org-pretty-entities-include-sub-superscripts nil)

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

    ;; `org-adapt-indentation' indents heading contents to the beginning of the heading. This is nice
    ;; in a way, but limits the amount of horizontal space when you have deeply-nested headings.
    (setq org-adapt-indentation nil)
    (setq org-log-done 'time)
    (setq org-todo-keywords
          '((sequence "HOLD" "TODO" "|" "FILE" "DONE" "CANCELLED")))
    (setq org-capture-templates
          '(("b" "pdf" entry (file "~/doc/notes/wiki.org")
             "* %f
:PROPERTIES:
:NOTER_DOCUMENT: %F
:END:
* outline
%(mh/pdf-outline-to-org-headline \"%F\" 1)")
            ("p" "productivity" entry (file+headline "~/doc/notes/projects/productivity.org" "refile")
             "* TODO %^{PROMPT}")
            ("w" "work" entry (file+headline "~/doc/notes/projects/work.org" "refile")
             "* TODO %^{PROMPT}")))
    (setq org-agenda-files '("~/doc/notes/wiki"))
    ;; use the current file for refile
    (setq org-refile-targets '((nil . (:maxlevel . 100))))
    ;; show candidates as slash-delimited (i.e. science/physics)
    (setq org-refile-use-outline-path t)
    ;; allows helm to get all completion candidates
    (setq org-outline-path-complete-in-steps nil)
    ;; speed up refile
    (setq org-refile-use-cache t)
    (setq org-agenda-follow-mode t)
    ;; include plain lists in org cycling, which folds lists by default when a heading is first
    ;; expanded.
    (setq org-cycle-include-plain-lists 'integrate)
    ;; Set file for the current entry.
    (defun org-set-property-file (file)
      (interactive
       (list
        (read-file-name "file: " "~/library/")))
      (org-set-property "Filepath" (concat "[[file:" file "]]")))
    ;; Outline percentage completion includes all children of node rather than just the direct
    ;; children.
    (setq org-checkbox-hierarchical-statistics nil)

    (setq org-format-latex-header "% Needed for proper rendering with some corner cases in luatex
\\RequirePackage{luatex85}
\\PassOptionsToPackage{usenames}{xcolor}
\\documentclass[border={0pt 1pt}]{standalone}
\[PACKAGES]
\[DEFAULT-PACKAGES]
%% Declared math operators
\\usepackage{math_local}")


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
                 '("" "xcolor" t))

    ;; make clocking efforts persistant across emacs sessions.
    ;; see [[info:org#Clocking%20Work%20Time][info:org#Clocking Work Time]]
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
    ;; Don't automatically attempt to resolve open clocks when
    ;; clocking in. Functionally, this is a nice feature, but it
    ;; creates a significant delay when there are many agenda
    ;; files. The proper solution seems to be to call
    ;; `org-resolve-clocks' manually.
    (setq org-clock-auto-clock-resolution nil)

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

    ;; ;; lualatex preview
    (setq org-latex-pdf-process
          '("latexmk -f -interaction=nonstopmode -output-directory=%o %f"))

    (setq luasvgm
          `(luasvgm :programs ("latexmk" "lualatex" "dvisvgm")
                    :description "pdf > svg"
                    :message "you need to install latexmk, lualatex and dvisvgm."
                    :use-xcolor t
                    :image-input-type "pdf"
                    :image-output-type "svg"
                    ;; The 72 / 200 corrects for the fact that DVISVGM
                    ;; uses pt units. It gives us 72 PPI but we want
                    ;; 200 DPI. Then, we want to upscale the image by
                    ;; 3x.
                    :image-size-adjust (,(/ (* 2.0 72.0) 200.0)  . ,(/ (* 2.0 72.0) 200.0))
		    :latex-compiler ("pdflatex -interaction nonstopmode -output-directory %o %f")
                    :image-converter ("dvisvgm --pdf -n -b min -c %S -o %O %f")))

    (add-to-list 'org-preview-latex-process-alist luasvgm)
    (setq org-preview-latex-default-process 'luasvgm)

    (load (concat user-emacs-directory "layers/org/babel.el"))

    ;; export macros
    (setq org-export-global-macros
          '((comment . "")))
    ;; export asynchronously
    (setq org-export-in-background t)

    ;; fontify latex fragments (inline latex) natively
    (setq org-highlight-latex-and-related '(native))

    ;; list of programs to use for opening links from org-mode
    (setq org-file-apps '((auto-mode . emacs)
                          ("\\.mm\\'" . default)
                          ("\\.x?html?\\'" . default)
                          ("\\.pdf\\'" . default)
                          ("\\.gif\\'" . (lambda (file link)
                                           (let ((my-image (create-image file))
                                                 (tmpbuf (get-buffer-create "*gif")))
                                             (switch-to-buffer tmpbuf)
                                             (erase-buffer)
                                             (insert-image my-image)
                                             (call-interactively 'image-mode)
                                             (image-animate my-image))))))

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
       ;; (ein . t)
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
       (sagemath . t)
       (sass . t)
       (scheme . t)
       (screen . t)
       (shell . t)
       (shen . t)
       (spice . t)
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

    ;; don't bastardize windows when editing a source block
    (setq org-src-window-setup 'other-window)

    ;; make contrib files visible
    ;; TODO modify this for nixpkgs
    (add-to-list 'load-path (concat user-emacs-directory "straight/repos/org/contrib/lisp") t))

  (use-package org-fragtog
    :config
    (add-hook 'org-mode-hook 'org-fragtog-mode))

  (use-package org-eldoc)

  ;; TODO doesn't work
  ;; (use-package org-edit-latex
  ;;   :config
  ;;   (add-hook 'org-mode-hook 'org-edit-latex-mode))

  ;; (use-package org-drill
  ;;   :config
  ;;   (setq org-drill-hide-item-headings-p t))

  ;; links
  (use-package ol
    :config
    (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                                 (vm-imap . vm-visit-imap-folder-other-frame)
                                 (gnus . org-gnus-no-new-news)
                                 (file . find-file-other-window)
                                 (wl . wl-other-frame))))

  (use-package org-edna
    :config
    (org-edna-load))

  (use-package ob-sagemath
    :config
    (setq org-babel-default-header-args:sage '((:session . t)
                                               (:results . "output"))))

  (load (concat user-emacs-directory "layers/org/export.el"))

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

  (defun mh/org-get-truncated-clock-string ()
    "Adapted from `org-clock-get-clock-string'. This just gets
rid of the headline, which takes too much space."
    (let ((clocked-time (org-clock-get-clocked-time)))
      (if org-clock-effort
	  (let* ((effort-in-minutes (org-duration-to-minutes org-clock-effort))
	         (work-done-str
		  (propertize (org-duration-from-minutes clocked-time)
			      'face
			      (if (and org-clock-task-overrun
				       (not org-clock-task-overrun-text))
				  'org-mode-line-clock-overrun
			        'org-mode-line-clock)))
	         (effort-str (org-duration-from-minutes effort-in-minutes)))
	    (format (propertize " [%s/%s]" 'face 'org-mode-line-clock)
		    work-done-str effort-str))
        (format (propertize " [%s]" 'face 'org-mode-line-clock)
	        (org-duration-from-minutes clocked-time)))))

  :postsetup
  (:layer pdf
   (defun mh/heading-filepath ()
     (file-truename
      (car (s-split "]" (car (last (s-split "file:" (org-entry-get (point) "NOTER_DOCUMENT" t))))))))

   (defun mh/open-book-from-outline ()
     (interactive)
     (setq file (mh/heading-filepath))
     (setq page (string-to-number (org-entry-get (point) "NOTER_PAGE" t)))
     (find-file-other-window file)
     (pdf-view-goto-page page))

   (defun mh/pdf-outline-to-org-headline (file base-depth)
     "Return a set of org headings from a pdf in FILE.
BASE-DEPTH is the depth (i.e. number of '*') of the destination
org file headline, and TODOP asks whether we should turn the
outline into a set of todo entries. Set 1 for yes and 0 for no.

Do not call this directly! It will simply discard the result. Use
org-capture instead."
     (interactive
      "fPDF file: \nnHeadline Depth: \n")
     (let ((outline (pdf-info-outline file))
           (org-outline ""))
       (dolist (item outline)
         (let-alist item
           (setq i (+ .depth base-depth))
           (while (> i 0)
             (setq org-outline (concat org-outline "*"))
             (setq i (- i 1)))
           (setq org-outline (concat org-outline " HOLD " .title "\n"))
           (setq org-outline (concat org-outline ":PROPERTIES:\n"
                                     ":NOTER_PAGE: " (number-to-string .page) "\n"
                                     ":END:\n"))))
       org-outline)))

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
   (setq evil-respect-visual-line-mode t)
   ;; evil appears to override certain org-mode keybindings with the
   ;; outline-mode counterparts. revert them here.
   (general-define-key
    :keymaps 'org-mode-map
    :states '(normal motion)
    "<tab>" 'org-cycle
    "g p" 'org-mark-ring-goto
    "g d" 'org-open-at-point
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "$" 'evil-end-of-line-or-visual-line
    "0" 'evil-beginning-of-visual-line
    "V" 'evil-visual-screen-line)

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
   ;; TODO add local keymaps
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
     "e" 'org-edit-special
     "c" 'org-ctrl-c-ctrl-c
     "P" 'org-priority
     "L" 'org-latex-preview
     "z" 'mh/command-org-tex-insert-prefix
     "o" 'org-open-at-point
     "R" 'org-table-iterate-buffer-tables)

   (general-define-key
    :keymaps 'org-mode-map
    "TAB" 'org-cycle
    "C-RET" 'org-insert-heading-respect-content)

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

  ;; (:layer internet
  ;;  (use-package org-eww))

  (:layer documentation
   (use-package ol-man))

  ;; (:layer windows
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\*Org Src.*" . (mh//display-popup-buffer-respect-monitors . ())))
  ;; )

  :func
  (setq mh-latex-scale 1.0)
  (defun mh/increase-latex-scale ()
    (interactive)
    (setq mh-latex-scale (+ mh-latex-scale 0.1))
    (call-interactively 'revert-buffer))

  (defun mh/decrease-latex-scale ()
    (interactive)
    (setq mh-latex-scale (- mh-latex-scale 0.1))
    (call-interactively 'revert-buffer))

  (defun mh/org-set-property-from-link ()
    (interactive)
    (org-set-property (org-read-property-name)
                      (org-insert-link)))

  (defun mh/org-clear-cache ()
    (interactive
     (org-refile-cache-clear)))

  (defun mh/org-mktmp (&optional fname)
    "Make a temporary directory and return the path of that
directory plus `FNAME', if `FNAME' is provided. If not simply
return the directory path. The temporary directory is given a
unique name based on the full org header path. This is meant as a
convenient way to create temporary directories for noweb babel
files from an org buffer."
    (let* ((tmpdir (sha1 (mapconcat 'identity (org-get-outline-path t) "/")))
           (dir (concat "/tmp/" tmpdir)))
      (mkdir dir t)
      (if fname
          (concat dir "/" fname)
        dir)))

  (defun mh/org-toggle-hide-emphasis-markers ()
    (interactive)
    "Toggle `org-hide-emphasis-markers'."
    (if (eq t org-hide-emphasis-markers)
        (setq org-hide-emphasis-markers nil)
      (setq org-hide-emphasis-markers t)))

  (defun mh//org-pdf-outline-headline-to-noter-format (node)
    "Convert the old outline format (with trailing page number in parentheses) to new noter page format.
NODE is the node representing the headline node."
    (let* ((headline-text (org-ml-get-property :raw-value node))
           (begin (org-ml-get-property :begin node))
           (match-position (string-match "\\(.*\\) (\\([1-9]+\\))" headline-text)))
      (if match-position
          (let ((headline-no-number (substring headline-text (match-beginning 1)
                                               (match-end 1)))
                (page-number (substring headline-text (match-beginning 2)
                                        (match-end 2))))
            (goto-char begin)
            (org-edit-headline headline-no-number)
            (org-set-property "NOTER_PAGE" page-number)
            (message (concat "Converted legacy headline `"
                             headline-no-number
                             "' "
                             "with number `"
                             page-number
                             "' to noter format."))))))

  (defun mh/update-all-pdf-outline-headlines-from-legacy-to-noter ()
    "Convert all headlines from the legacy format in which the
page number is a trailing number in parentheses to the new org
noter format.

TODO this works but is slow."
    (interactive)
    (org-api/map-nodes-recursive-in-current-buffer
     'mh//org-pdf-outline-headline-to-noter-format
     '(headline)))

  (defun mh/org-adapt-line-length-to-visual-line-mode ()
    "Remove line breaks in org-mode inserted by fill-column."
    (interactive)
    (setq-local fill-column 1000000)
    (org-api/map-nodes-recursive-in-current-buffer
     (lambda (node)
       (let ((begin (org-ml-get-property :begin node)))
         (goto-char begin)
         (org-fill-paragraph)))
     '((:or paragraph item))))

  (defun mh/org-replace-common-legacy-symbols-in-current-buffer ()
    ""
    (interactive)
    (mh/replace-all-alist-items-in-current-buffer
     '(("_{in}" . "_{\\\\mathrm{in}}")
       ("_{out}" . "_{\\\\mathrm{out}}")
       ("_{\\\\mathit{CC}}" . "_{\\\\mathrm{CC}}")
       ("_{CC}" . "_{\\\\mathrm{CC}}")
       ("_{EE}" . "_{\\\\mathrm{EE}}")
       ("_{CE}" . "_{\\\\mathrm{CE}}")
       ("_{BE}" . "_{\\\\mathrm{BE}}")
       ("V_T" . "V_{\\\\mathrm{T}}")
       ("V_A" . "V_{\\\\mathrm{A}}")
       ("V_E" . "V_{\\\\mathrm{E}}")
       ("R_E" . "R_{\\\\mathrm{E}}")
       ("I_E" . "I_{\\\\mathrm{E}}")
       ("V_B" . "V_{\\\\mathrm{B}}")
       ("R_B" . "R_{\\\\mathrm{B}}")
       ("I_B" . "I_{\\\\mathrm{B}}")
       ("V_C" . "V_{\\\\mathrm{C}}")
       ("R_C" . "R_{\\\\mathrm{C}}")
       ("I_C" . "I_{\\\\mathrm{C}}")
       ("_{load}" . "_{\\\\mathrm{load}}")
       ("_{source}" . "_{\\\\mathrm{source}}")
       (",>=stealth" . ""))))

  (defun mh/org-insert-file-image (file)
    "Insert an inline image at point from FILE into an Org buffer."
    (interactive "fFile: ")
    (insert
     (concat "#+ATTR_ORG: :width 1000\n"
             "#+ATTR_HTML: :width 100%\n"
             "#+NAME: fig:\n"
             "[[file:"
             (file-relative-name file)
             "]]"))
    (org-display-inline-images))

  (defun mh/org-insert-sections-personal-machine ()
    "Insert sections for a personal machine wiki page."
    (interactive)
    (insert
     (concat "* specifications\n"
             "* options\n"
             "* operating instructions\n"
             "** installation\n"
             "** interface\n"
             "*** front panel\n"
             "**** connectors\n"
             "*** back panel\n"
             "**** connectors\n"
             "* theory of operation\n"
             "** hardware\n"
             "** software\n"
             "* applications\n"
             "* construction\n"
             "** assembly locator\n"
             "** circuit assemblies\n"
             "*** components\n"
             "*** component locator\n"
             "*** images\n"
             "** exploded views\n"
             "* assembly and disassembly\n"
             "* functional verification\n"
             "* troubleshooting\n"
             "* maintenance\n"
             "** preliminary maintenance\n"
             "*** non-volatile storage backup\n"
             "*** components that require immediate replacement\n"
             "** general maintenance\n"
             "*** component aging and failure\n"
             "*** cleaning\n"
             "* calibration and adjustment\n"
             "* accessories\n"
             "* modifications\n"
             "* machines\n"
             "** status\n"
             "** options\n"
             "** initial instrument state\n"
             "*** visual inspection\n"
             "**** exterior\n"
             "**** interior\n"
             "** functional verification log\n"
             "** calibration and adjustment log\n"
             "** repair log\n"
             "** modifications\n")))

  (defun mh/org-insert-sections-circuit ()
    "Insert sections for a circuit wiki page."
    (interactive)
    (insert
     (concat "** schematic\n"
             "** operation\n"
             "** configuration\n"
             "** simulation\n"
             "** limitations\n"
             "** alternatives\n"
             "** variants\n"
             "** references\n"
             "** glossary\n")))

  ;; TODO doesn't quite work with org-fragtog yet.
  (defun mh/org-open-latex-fragment-file-at-point ()
    "Open the file storing the latex fragment at point."
    (interactive)
    (let ((pt (point)))
      ;; If we're using a package like org-fragtog, we won't know we're
      ;; at an overlay if we keep the cursor positioned over it.
      (save-excursion
        (org-back-to-heading-or-point-min)
        (org-display-inline-images t t)
        ;;(goto-char 0)
        (let ((ov (overlays-at pt)))
          (if (eq nil ov)
              (message "There is no overlay at the current position.")
            (let ((image (get-char-property pt 'display)))
              (find-file (image-property image :file)))))))))

(provide 'org-layer)
;;; org-layer.el ends here
