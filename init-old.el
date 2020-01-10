;; init.el --- Summary: -*-no-byte-compile: t; -*-

;;; Code:

;; Enable line numbers on the LHS
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'plain-TeX-mode-hook 'display-line-numbers-mode)

;; Keybindings for image mode
(add-hook 'image-mode-hook
          (lambda ()
            (local-set-key (kbd "C-\+") 'image-increase-size)))
(add-hook 'image-mode-hook
          (lambda ()
            (local-set-key (kbd "C-\-") 'image-decrease-size)))

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Specify buffers that should appear in the same window
(add-to-list 'same-window-buffer-names "*Faces*")
(add-to-list 'same-window-regexps ".*ein.*")

;; Settings for searching
;; case insensitive searches by default
(setq-default case-fold-search t)
;; highlight matches when searching
(setq-default search-highlight t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq
 linux-header-path (concat "/lib/modules/" (shell-command-to-string "echo -n $(uname -r)") "/build/include"))

;; Shortcuts for opening frequently visited files.
(global-set-key (kbd "C-c f i") (lambda ()
                                  (interactive)
                                  (find-file user-init-file)))
(global-set-key (kbd "C-c f l") (lambda ()
                                  (interactive)
                                  (find-file org-books-file)))

;; Utility to find keybinding in all modes.
(with-eval-after-load "s"
  (defun list-known-bindings (key)
    (interactive "kList known bindings for key: ")
    (with-current-buffer (get-buffer-create "*known bindings*")
      (erase-buffer)
      (mapatoms (lambda (sym)
                  (when (or (eq sym 'global-map)
                            (and (boundp sym)
                                 (symbol-value sym)
                                 (s-ends-with-p "-mode-map" (symbol-name sym))
                                 (keymapp (symbol-value sym))))
                    (let ((binding (lookup-key (symbol-value sym) key t)))
                      (when (and binding
                                 (not (numberp binding)))
                        (insert (format "%-40s `%s'\n"
                                        (format "`%s'" sym)
                                        (if (keymapp binding)
                                            "KEYMAP"
                                          binding))))))))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (insert
       (format "Known bindings for key: %s\n\n" (key-description key))
       (format "%-40s %s" "Map" "Binding\n")
       (s-repeat 40 "-") " " (s-repeat 30 "-") "\n")
      (display-buffer (current-buffer)))))

;; Add external info manuals to those displayed by emacs info
(setq Info-additional-directory-list (list "/home/matt/.local/share/info"))

;; Install use-package with straight.
(straight-use-package 'use-package)
;; default :straight t
(setq straight-use-package-by-default t)

;;;; Built-in packages.

;; (use-package autoinsert
;;   :straight nil
;;   :hook
;;   ((find-file . auto-insert))
;;   :config
;;   (define-auto-insert '("\\.org'" . "org skeleton")
;;     '("#+title: "
;;       "")))

(use-package bash-completion
  :config
  (bash-completion-setup))

;; info+ is a plugin.
(straight-use-package '(info+ :local-repo "info+"))
(use-package info+)

;; Improve PDF resolution in DocView
(use-package doc-view
  :config
  (setq doc-view-resolution 192))

;; Load the compile command
;; Use "C-c i" in a compilation buffer to allow user input.
(use-package compile
  :config
  (setq compilation-scroll-output 'next-error)
  (setq compilation-skip-threshold 2)
  (defun endless/toggle-comint-compilation ()
    "Restart compilation with (or without) `comint-mode'."
    (interactive)
    (cl-callf (lambda (mode) (if (eq mode t) nil t))
        (elt compilation-arguments 1))
    (recompile))
  :bind (("C-c C-c" . (lambda (cmd)
			(interactive
			 (list
			  (compilation-read-command compile-command)))
			(compile cmd t)))
         :map compilation-mode-map
         ("C-c i" . endless/toggle-comint-compilation)
         :map compilation-minor-mode-map
         ("C-c i" . endless/toggle-comint-compilation)
         :map compilation-shell-minor-mode-map
         ("C-c i" . endless/toggle-comint-compilation)))

;; replace buffer-menu with ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (autoload 'ibuffer "ibuffer" "List buffers." t)
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats.
  (setq ibuffer-formats
        (quote
	 ((mark modified read-only " "
	        (name 50 50 :left :elide)
	        " "
	        (size-h 9 -1 :right)
	        " "
	        (mode 16 16 :left :elide)
	        " "
	        filename-and-process))))
  (setq ibuffer-saved-filter-groups
	(quote (("default"
		 ("PDFs" (mode . pdf-view-mode))
		 ("Notes" (mode . latex-mode))
		 ("org" (mode . org-mode))
		 ("gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))
		 ("dired" (mode . dired-mode))
		 ("planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . muse-mode)))
		 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
		 ))))
  (defadvice ibuffer-update-title-and-summary (after remove-column-titles)
    (save-excursion
      (set-buffer "*Ibuffer*")
      (read-only-mode 0)
      (goto-char 1)
      (search-forward "-\n" nil t)
      (delete-region 1 (point))
      (let ((window-min-height 1))
	;; save a little screen estate
	(shrink-window-if-larger-than-buffer))
      (read-only-mode)))

  (ad-activate 'ibuffer-update-title-and-summary))

(use-package bind-key)

(use-package paren
  :config
  (show-paren-mode t))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode t))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (save-place-mode t))

;; Save minibuffer history.
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode t))

;; ;; Maintains a menu of recently opened files.
;; (use-package recentf
;;   :functions recentf-mode
;;   :config
;;   (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
;;         recentf-max-saved-items 500
;;         recentf-max-menu-items 15
;;         ;; disable recentf-cleanup on Emacs start, because it can cause
;;         ;; problems with remote files
;;         recentf-auto-cleanup 'never)
;;   (recentf-mode t))

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package cl-lib)

(use-package multi-line
  :config
  (global-set-key (kbd "C-c d") 'multi-line))

;; Semantic provides better code completion than many other frameworks. It does this by using a
;; realtime source code parser. It can therefore provide contextual code completion. For instance,
;; if you include a header file, it will parse that header and customize the completions based on
;; what's available in that header. The obvious downside is that it can create a lag after a header
;; is included. If this becomes unbearable, consider disabling it.
;; (use-package semantic
;;   :config
;;   ;; Cache parses for faster future access.
;;   (global-semanticdb-minor-mode 1)
;;   ;; Check if buffer is outdated when user is idle. If so, reparse.
;;   (global-semantic-idle-scheduler-mode 1)
;;   (semantic-mode 1)
;;   ;; Additional include paths.
;;   (semantic-add-system-include "/usr/include/boost" 'c++-mode)
;;   (semantic-add-system-include linux-header-path))

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(use-package seq)

(use-package org
  :init
  (setq book-file "/home/matt/library/book-list.org")
  (setq emacs-file "/home/matt/.emacs.d/doc/emacs.org")
  (setq system-file "/home/matt/notes/computing/system/system.org")
  (defun open-book-from-outline ()
    (interactive)
    (setq file (car (s-split "]" (car (last (s-split "file:" (org-entry-get (point) "Filepath")))))))
    (setq page (string-to-number (car (s-match "[0-9]+"
                                               (car (s-match "([0-9]+)" (thing-at-point 'line t)))))))
    (find-file-other-window file)
    (pdf-view-goto-page page))

  :hook
  (((org-mode . (lambda ()
                  (if (equal buffer-file-name book-file)
                      (add-hook 'after-save-hook 'org-html-export-to-html nil t)))))
   (org-mode . (lambda ()
                 (setq-local fill-column 70))))
  ;; :bind (:map org-mode-map
  ;;             ("C-<return>" . open-book-from-outline))
  :config
  (setq org-startup-with-latex-preview t)
  (setq org-startup-with-inline-images t)

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (setq org-enforce-todo-dependencies t)

  ;; use habits
  (push 'org-habit org-modules)
  (setq org-habit-show-habits nil)

  ;; agenda view
  (setq org-agenda-custom-commands
        '(("c" "Custom agenda view"
           ((tags-todo "work"
                       ((org-agenda-overriding-header "Work")))
            (agenda "" ((org-agenda-span 1)))
            (tags-todo "productivity"
                       ((org-agenda-overriding-header "Productivity")))
            (tags-todo "read"
                       ((org-agenda-overriding-header "Read")))))))

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

  ;; allow customizing the image width
  (setq org-image-actual-width nil)

  ;; fontify when not in polymode
  (setq org-src-fontify-natively t)

  ;; preserve src block indentation
  ;; this works better with aggressive-indent-mode
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)

  ;; don't display emphasis characters
  (setq org-hide-emphasis-markers t)

  ;; permit still typing emphasis characters as normal characters
  ;; see https://emacs.stackexchange.com/a/16746/20317
  (defun modi/org-entity-get-name (char)
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

  (defun modi/org-insert-org-entity-maybe (&rest args)
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
        (setq entity-name (modi/org-entity-get-name pressed-key))
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

  ;; Run `org-self-insert-command' only if `modi/org-insert-org-entity-maybe'
  ;; returns nil.
  (advice-add 'org-self-insert-command :before-until #'modi/org-insert-org-entity-maybe)
  (setq org-pretty-entities t)

  ;; set inline code appearance
  (set-face-background 'org-code "#27323D")
  (set-face-foreground 'org-code "unspecified")

  ;; `org-adapt-indentation' indents heading contents to the beginning of the heading. This is nice
  ;; in a way, but limits the amount of horizontal space when you have deeply-nested headings.
  (setq org-adapt-indentation nil)
  (setq org-log-done 'time
        org-todo-keyword-faces '(("INPROGRESS" . (:foreground "#cc8800"))))
  (setq org-todo-keywords
        '((sequence "HOLD" "TODO" "STARTED" "|" "DONE" "CANCELLED")))
  ;; (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-capture-templates
        ;;         '(("b" "Book" entry (file book-file)
        ;;            "* %^{TITLE}\n\
        ;; :PROPERTIES:\n\
        ;; :Author:\n\
        ;; :Edition:\n\
        ;; :Pub-Year:\n\
        ;; :Filepath:\n\
        ;; :Goodreads-Ranking:\n\
        ;; :Goodreads-#-Reviews:\n\
        ;; :Amazon-Ranking:\n\
        ;; :Amazon-#-Reviews:\n\
        ;; :Goodreads-URL:\n\
        ;; :Amazon-URL:\n\
        ;; :OpenLibrary-URL:\n\
        ;; :ISBN:\n\
        ;; :END:\n%?" :empty-lines 1))
        '(("p" "pdf" entry (file "/home/matt/doc/notes/org/wiki.org")
           "* %f\n\
:PROPERTIES:\n\
:Filepath: %a\n\
:END:\n\
** description\n\
** outline [/]\n\
%(mh/pdf-outline-to-org-headline \"%F\" 2 t)")))
  ;; (setq org-agenda-files '("/media/samsung-t5/notes/org"))
  (setq org-agenda-files (directory-files-recursively "/home/matt/doc/notes/org" "org$"))
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 10))))
  (setq org-refile-use-outline-path t)
  ;; (setq org-agenda-files (list book-file emacs-file system-file))
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

  (add-to-list 'org-latex-packages-alist
               '("" "tikz" t))
  (add-to-list 'org-latex-packages-alist
               '("" "circuitikz" t))
  (setq org-latex-create-formula-image-program 'imagemagick)
  ;; necessary for drawing electronics circuits with tikz using the `circuits' library.
  ;; (setq org-format-latex-header
  ;;       (concat org-format-latex-header
  ;;               "\\usetikzlibrary{circuits.logic.US,circuits.logic.IEC,circuits.ee.IEC}"))

  ;; (plist-put org-format-latex-options :background "white")

  (setq org-latex-pdf-process (list "latexmk -f -pdf %f"))

  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (asymptote)
     (awk)
     (calc . t)
     (clojure . t)
     (comint)
     (css)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (fortran)
     (gnuplot . t)
     (haskell)
     (io)
     (java)
     (js . t)
     (latex . t)
     (ledger . t)
     (lilypond)
     (lisp . t)
     (lua . t)
     (matlab)
     (maxima)
     (mscgen)
     (ocaml)
     (octave . t)
     (org . t)
     (perl)
     (picolisp)
     (plantuml . t)
     (python . t)
     ;;     (ipython . t)
     ;;     (restclient . t)
     (ref)
     (ruby)
     (sass)
     (scala)
     (scheme)
     (screen)
     (shell . t)
     (shen)
     (snippet)
     (sql . t)
     (sqlite . t)))

  ;; org crypt
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key "huszaghmatt@gmail.com")

  ;; identify org headlines with UUIDs
  ;; see https://writequit.org/articles/emacs-org-mode-generate-ids.html
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (defun org-id-new (&optional prefix)
    "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org-4nd91V40HI\"."
    (let* ((prefix (if (eq prefix 'none)
                       ""
                     (concat (or prefix org-id-prefix) "-")))
           unique)
      (if (equal prefix "-") (setq prefix ""))
      (cond
       ((memq org-id-method '(uuidgen uuid))
        (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
        (unless (org-uuidgen-p unique)
          (setq unique (org-id-uuid))))
       ((eq org-id-method 'org)
        (let* ((etime (org-reverse-string (org-id-time-to-b36)))
               (postfix (if org-id-include-domain
                            (progn
                              (require 'message)
                              (concat "@" (message-make-fqdn))))))
          (setq unique (concat etime postfix))))
       (t (error "Invalid `org-id-method'")))
      (concat prefix unique)))

  (defun my/org-custom-id-get (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  (defun my/org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
        (org-map-entries (lambda () (my/org-custom-id-get (point) 'create))))))

  ;; automatically add ids to saved org-mode headlines
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (when (and (eq major-mode 'org-mode)
                                     (eq buffer-read-only nil))
                            (my/org-add-ids-to-headlines-in-file))))))

  ;; automatically add ids to captured headlines
  (add-hook 'org-capture-prepare-finalize-hook
            (lambda () (my/org-custom-id-get (point) 'create)))

  ;; make contrib files visible
  (add-to-list 'load-path "~/.emacs.d/straight/repos/org/contrib/lisp" t))

(use-package org-man
  :straight nil
  :after org)

(use-package lsp-mode
  :hook (prog-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
;; ;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package rainbow-mode)

(use-package geiser
  :config
  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/src/guix")))

;; Spelling correction.
(use-package flyspell
  :bind (("<f6>" . flyspell-buffer)
	 ("<f7>" . flyspell-correct-at-point))
  :functions (flyspell-mode flyspell-goto-next-error)
  :hook ((text-mode . flyspell-mode)
         (org-mode . flyspell-mode))
  :init
  (setq flyspell-issue-welcome-flag nil)
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)))

;; Provide better bindings for shell commands.
(global-set-key (kbd "C-c s") 'with-editor-async-shell-command)
(global-set-key (kbd "C-c S") (lambda (cmd) (interactive "sSudo async shell command: ")
                                (let ((default-directory "/sudo::"))
                                  (with-editor-async-shell-command cmd))))

(use-package shell
  :bind (:map shell-mode-map
         ("C-d" . nil)))

(defun updatedb ()
  "Update mlocate database."
  (interactive)
  (let ((default-directory "/sudo::"))
    (async-shell-command "updatedb")))

(defun upgrade-software ()
  "Use pacman to upgrade installed software."
  (interactive)
  (let ((default-directory "/sudo::"))
    (async-shell-command "pacman -Syu --ignore vtk")))

(defun sudo-locate (pattern)
  "Locate files with superuser privileges."
  (interactive
   (list
    (read-string "pattern: ")))
  (let ((default-directory "/sudo::"))
    (async-shell-command (format "locate %s" pattern))))

(use-package image-dired
  :config
  (setq image-dired-thumb-size 1000))

(defun ediff-copy-both-to-C ()
  "Copy both changes to the merge resolution buffer."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; .dcm files are opening in imagemagick mode, they should open in fundamental mode.
(add-to-list 'auto-mode-alist '("\\.dcm\\'" . fundamental-mode))

(use-package bookmark
  :config
  ;; save bookmarks to file whenever they are updated.
  (setq bookmark-save-flag 1))

;;;; Third-party packages.

(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  ;; enable switching current buffer with new buffer. this stops a bunch of buffers being added when
  ;; a file/dir is navigated to in dired mode.
  (put 'dired-find-alternate-file 'disabled nil))

;; (use-package bookmark+)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; pdf-tools provides better pdf display
(defun pdf-view--rotate (&optional counterclockwise-p page-p)
  "Rotate PDF 90 degrees. Requires pdftk to work.\n
Clockwise rotation is the default; set COUNTERCLOCKWISE-P to
non-nil for the other direction.  Rotate the whole document by
default; set PAGE-P to non-nil to rotate only the current page.
\nWARNING: overwrites the original file, so be careful!"
  ;; error out when pdftk is not installed
  (if (null (executable-find "pdftk"))
      (error "Rotation requires pdftk")
    ;; only rotate in pdf-view-mode
    (when (eq major-mode 'pdf-view-mode)
      (let* ((rotate (if counterclockwise-p "left" "right"))
             (file   (format "\"%s\"" (pdf-view-buffer-file-name)))
             (page   (pdf-view-current-page))
             (pages  (cond ((not page-p)                        ; whole doc?
                            (format "1-end%s" rotate))
                           ((= page 1)                          ; first page?
                            (format "%d%s %d-end"
                                    page rotate (1+ page)))
                           ((= page (pdf-info-number-of-pages)) ; last page?
                            (format "1-%d %d%s"
                                    (1- page) page rotate))
                           (t                                   ; interior page?
                            (format "1-%d %d%s %d-end"
                                    (1- page) page rotate (1+ page))))))
        ;; empty string if it worked
        (if (string= "" (shell-command-to-string
                         (format (concat "pdftk %s cat %s "
                                         "output %s.NEW "
                                         "&& mv %s.NEW %s")
                                 file pages file file file)))
            (pdf-view-revert-buffer nil t)
          (error "Rotation error!"))))))

(defun pdf-view-rotate-clockwise (&optional arg)
  "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
entire document."
  (interactive "P")
  (pdf-view--rotate nil (not arg)))

(defun pdf-view-rotate-counterclockwise (&optional arg)
  "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
rotate entire document."
  (interactive "P")
  (pdf-view--rotate :counterclockwise (not arg)))

;; workaround for pdf-tools not reopening to last-viewed kage of the pdf:
;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
(defun pdf-set-last-viewed-bookmark ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (bookmark-set (pdf-generate-bookmark-name))))

(defun pdf-jump-last-viewed-bookmark ()
  (bookmark-set "fake") ; this is new
  (when
      (pdf-has-last-viewed-bookmark)
    (bookmark-jump (pdf-generate-bookmark-name))))

(defun pdf-has-last-viewed-bookmark ()
  (assoc
   (pdf-generate-bookmark-name) bookmark-alist))

(defun pdf-generate-bookmark-name ()
  (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

(defun pdf-set-all-last-viewed-bookmarks ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (pdf-set-last-viewed-bookmark))))

;; TODO add hook to set bookmark for file whenever there is a page change.
(use-package pdf-tools
  :mode "\\.pdf\\'"
  :bind (("<next>" . next-buffer)
         ("<prior>" . previous-buffer)
         :map pdf-view-mode-map
         ("j" . pdf-view-next-line-or-next-page)
         ("k" . pdf-view-previous-line-or-previous-page))
  :config
  (pdf-tools-install)

  (defun mh/pdf-outline-to-org-headline (file base-depth todop)
    "Return a set of org headings from a pdf in FILE.
BASE-DEPTH is the depth (i.e. number of '*') of the destination
org file headline, and TODOP asks whether we should turn the
outline into a set of todo entries. Set 1 for yes and 0 for no."
    (interactive)
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

(add-hook 'kill-buffer-hook 'pdf-set-last-viewed-bookmark)
(add-hook 'kill-emacs-hook #'pdf-set-all-last-viewed-bookmarks)
(add-hook 'pdf-view-mode-hook 'pdf-jump-last-viewed-bookmark)

(use-package a)

;; library for async/thread processing
(use-package async)

(use-package pandoc-mode)

;; Looks for bugs in elisp files.
(use-package bug-hunter)

;; Startup profiler.
(use-package esup
  :config
  (autoload 'esup "esup" "Emacs Start Up Profiler." nil))

;; Enable plantuml-mode for PlantUML files
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :config
  ;; plantuml_helpers is a plugin file.
  ;; plantuml-mode needs to know where the executable file is.
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(straight-use-package '(plantuml-helpers :local-repo "plantuml-helpers"))
(use-package plantuml-helpers
  :after (plantuml-mode))

(use-package flycheck-plantuml
  :after (flycheck plantuml-mode))

(use-package google-this
  :config
  (google-this-mode 1))

(use-package s)

;; provides additional list functionality.
(use-package dash
  :straight dash)

(use-package dash-docs
  :straight (dash-docs :type git :host github :repo "dash-docs-el/dash-docs")
  :config
  (setq dash-docs-browser-func 'eww)
  (defmacro set-docset (hook &rest docsets)
    (list 'add-hook (list 'quote hook)
          (list 'lambda ()
                (list 'setq-local 'dash-docs-docsets (list 'quote docsets)))))
  (set-docset emacs-lisp-mode-hook "Emacs Lisp")
  (set-docset c-mode-hook "C")
  (set-docset c++-mode-hook "C" "C++")
  (set-docset cmake-mode-hook "CMake")
  (set-docset python-mode-hook "Python 3")
  (set-docset css-mode-hook "CSS")
  (set-docset html-mode-hook "HTML"))

(use-package helm-dash
  :after dash-docs)

;; Use Helm interface to GNU Global.
(use-package helm-gtags
  :after (helm)
  :init (setq helm-gtags-prefix-key "\C-cg"
              helm-gtags-ignore-case t
              helm-gtags-auto-update t
              helm-gtags-use-input-at-cursor t
              helm-gtags-pulse-at-cursor t
              helm-gtags-suggested-key-mapping t)
  :hook ((dired-mode . helm-gtags-mode)
         (eshell-mode . helm-gtags-mode)
         (c-mode-common . helm-gtags-mode)
         (asm-mode . helm-gtags-mode))
  :bind (("C-c g a" . helm-gtags-tags-in-this-function)
         ("C-j" . helm-gtags-select)
         ("C-c <" . helm-gtags-previous-history)
         ("C-c >" . helm-gtags-next-history)))

;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep)

;; Allows editing of things in Chrome/Firefox with Emacs.
(use-package edit-server
  :functions edit-server-start
  :config
  (progn
    (when (daemonp)
      (edit-server-start))
    (add-hook 'edit-server-start-hook
              (lambda ()
                (when (string-match "github.com" (buffer-name))
                  (markdown-mode))))))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar))

;; A frontend for interfacing to external debuggers.
(use-package realgud
  :init
  (setenv "TERM" "dumb")
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;; Provides better C++ code highlighting.
(use-package modern-cpp-font-lock
  :functions modern-c++-font-lock-global-mode
  :config
  (modern-c++-font-lock-global-mode t))

;; Allows better interaction with ElasticSearch.
(use-package es-mode
  :mode "\\.es$")

;; Code completions.
(setq default-company-backends '(company-semantic
                                 company-gtags
                                 company-files
                                 company-keywords
			         company-capf
			         company-yasnippet
			         company-abbrev
                                 company-dabbrev
                                 company-dabbrev-code))

(use-package company
  :bind
  ("C-<return>" . company-complete-selection)
  :config
  ;; show completions immediately
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (global-company-mode)
  ;; Maintain case information for completions.
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  ;; Default backends.
  (setq company-backends (list default-company-backends)))

(use-package company-c-headers
  :config
  ;; Include C++ header files in completions.
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.2.1/" t)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.2.1/x86_64-pc-linux-gnu/" t)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.2.1/backward/" t)
  ;; Include Linux header files in completions.
  (add-to-list 'company-c-headers-path-system linux-header-path t))

;; Completions for references in AUCTeX
(use-package company-reftex
  :after company)

;; Provides math completions in AUCTeX
(use-package company-math
  :after company)

;; Completions for AUCTeX.
(use-package company-auctex
  :after (auctex company yasnippet))

;; Provides python completion
(use-package company-jedi
  :after (python company))
  ;; :config
  ;; (jedi:install-server))

(use-package exwm
  :config
  (require 'exwm-config)
  (exwm-enable)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "eDP-1-1" 1 "DP-0" 2 "DP-2"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr1" nil "xrandr --output eDP-1-1 --right-of DP-0 --auto")))
  ;; "xrandr1" nil "xrandr --output DP-0 --left-of eDP-1-1 --auto")))
  ;;              (start-process-shell-command
  ;;               "xrandr2" nil "xrandr --output DP-2 --right-of eDP-1-1 --auto")))
  (exwm-randr-enable))

(use-package nix-mode
  :mode "\\.nix\\'")

;; Code syntax checking.
(use-package flycheck
  :defer t
  :functions global-flycheck-mode
  :init
  :bind ("C-c l" . flycheck-list-errors)
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  (setq flycheck-clang-language-standard nil)
  ;; Verilog Verilator static analyzer
  (setq-default flycheck-verilog-verilator-executable "/usr/bin/verilator_bin"))

;; Rtags
;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
;; (defun setup-flycheck-rtags ()
;;   (interactive)
;;   (flycheck-select-checker 'rtags))

(use-package rtags
  :after company
  :bind (:map c-mode-base-map
         ("M-." . rtags-find-symbol-at-point)
         ("M-," . rtags-location-stack-back)
         ("M-r" . rtags-find-references-at-point))
  :hook ((c-mode-common . rtags-start-process-unless-running))
  :config
  (rtags-enable-standard-keybindings)
  (setq rtags-autostart-diagnostics t
        rtags-completions-enabled t
        rtags-use-helm t)
  (rtags-diagnostics))

;; Common lisp package.
(use-package slime
  :init
  (require 'slime-autoloads)
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(use-package slime-company
  :after (slime company)
  :config
  (slime-setup '(slime-fancy slime-company)))

(use-package cmake-ide
  :after rtags
  :config
  (cmake-ide-setup))

;; Mode for interacting with systemd files.
(use-package systemd)

(use-package helm-systemd)

;; Completions in octave.
(use-package ac-octave
  :hook ((octave-mode . (lambda ()
                          (company-mode -1)))
         (octave-mode . (lambda ()
                          (auto-complete-mode 1)))
         (octave-mode . (lambda ()
                          (ac-octave-setup)))))

(global-set-key (kbd "C-x g") 'magit-status)

(use-package magit-org-todos
  :config
  (magit-org-todos-autoinsert))

;; Access GNU bug tracker from within Emacs.
(use-package debbugs
  :straight (debbugs :type git :host github :repo "emacsmirror/debbugs"
                     :files (:defaults "Debbugs.wsdl")))

;; Major mode for editing PKGBUILD files.
(use-package pkgbuild-mode)

;; Have Emacs inherit PATH.
(use-package exec-path-from-shell
  :init
  ;; Remove environment variables warning.
  (setq exec-path-from-shell-check-startup-files nil)
  ;; Allows ssh-agent password caching use in Emacs.
  (exec-path-from-shell-copy-envs '("SSH_AGENT_PID"
                                    "SSH_AUTH_SOCK"))
  :config
  (exec-path-from-shell-initialize))

;; Interact with MPD.
(use-package libmpdel)

(use-package mpdel
  :after libmpdel
  :config
  (mpdel-mode))

(use-package djvu)

;; Fast, interactive greping.
(use-package deadgrep
  :after (dash s spinner))
;;:bind ("<return>" . deadgrep-visit-result-other-window))

(global-set-key (kbd "<f5>") 'deadgrep)

;; Easy radix conversion
(use-package 0xc)

;; Query REST sites.
(use-package restclient)

(use-package company-restclient
  :after (company cl-lib restclient know-your-http-well)
  :hook (restclient-mode . (lambda ()
                             (set (make-local-variable 'company-backends)
                                  (list
                                   (cons 'company-restclient default-company-backends))))))

(use-package request)

;; Usability improvements for LaTeX.
(use-package latex-extra
  :after (auctex cl-lib))

;; Shortcuts for difficult-to-type text.
(use-package yasnippet
  :commands (yas-reload-all)
  :functions yas-global-mode
  :defer 5
  :config
  (yas-global-mode t)
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yas-reload-all))

(use-package yaml-mode
  :mode (".yml" ".yaml"))

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)))

;; Open large files.
(use-package vlf)

;; Unit tests for elisp.
(use-package test-simple)

;; Switch between different cases, eg CamelCase, lowerCamelCase, snake_case, and
;; SCREAMING_SNAKE_CASE
(use-package string-inflection
  :defer t
  :bind (("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase)))

;; Interface to virtual environment.
(use-package pyvenv)

(use-package json-mode
  :after (origami)
  :mode ("\\.json\\'" "\\.imp\\'")
  ;; Origami allows folding, which is really useful when viewing json.
  :hook (json-mode . origami-mode))

(use-package origami)

;; Delete all whitespace until the next whitespace character.
(use-package hungry-delete
  :functions global-hungry-delete-mode
  :config
  (global-hungry-delete-mode t))

(use-package flyspell-correct)
;; Use helm for flyspell interface.
(use-package flyspell-correct-helm
  :after flyspell-correct
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package ein
  :init
  (defvar my-jupyter-location (executable-find "jupyter"))
  (defvar my-jupyter-start-dir "/home/matt/.jupyter")
  :bind (("<f2>" . ein:connect-to-notebook)
         :map ein:connect-mode-map
         ;; Preserve "M-," for jumping back after jumping to a definition, etc.
         ("M-," . nil))
  :hook (ein:notebook-multilang-mode . (lambda ()
                                         (display-line-numbers-mode -1)))
  :config
  (require 'ein-notebook)
  (require 'ein-subpackages)
  (setq ein:polymode t)
  (ein:run my-jupyter-location my-jupyter-start-dir)
  ;; (ein:jupyter-server-start my-jupyter-location my-jupyter-start-dir)
  )

(use-package mediawiki)

(use-package cuda-mode
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

(use-package dockerfile-mode
  :mode ("Dockerfile"))

(use-package multi-term
  :after (evil)
  :bind (("<C-next>" . multi-term-next)
         ("<C-prior>" . multi-term-prev)
         ("C-c t" . multi-term-next)
         ("<f1>" . multi-term))
  :config
  (setq multi-term-program "/bin/bash")
  (setq term-bind-key-alist
        (list (cons "C-c C-c" 'term-interrupt-subjob)
              (cons "C-c C-e" 'term-send-esc)
              (cons "C-p" 'previous-line)
              (cons "C-n" 'next-line)
              (cons "C-z" 'term-stop-subjob)
              (cons "C-l" 'term-send-raw)
              (cons "C-s" 'term-send-raw)
              (cons "C-r" 'term-send-raw)
              (cons "C-m" 'term-send-return)
              (cons "C-y" 'term-paste)
              (cons "M-f" 'term-send-forward-word)
              (cons "M-b" 'term-send-backward-word)
              (cons "M-DEL" 'term-send-backward-kill-word)
              (cons "M-d" 'term-send-forward-kill-word)
              (cons "<C-left>" 'term-send-backward-word)
              (cons "<C-right>" 'term-send-forward-word)
              ;; Keep evil mode window move keys. This replaces the normal Bash C-w key that cuts
              ;; the word before the cursor. If you want that back, remove the next 5 lines.
              (cons "C-w" 'nil)
              (cons "C-w h" 'evil-window-left)
              (cons "C-w j" 'evil-window-down)
              (cons "C-w k" 'evil-window-up)
              (cons "C-w l" 'evil-window-right))))

(straight-use-package '(multi-term-ext :local-repo "multi-term-ext"))
(use-package multi-term-ext)

(use-package asm-mode
  :mode ("\\.s\\'"))

;; These are required for syslog.
(use-package hide-lines)
(use-package ov)

;; syslog-mode is a plugin for viewing log files.
(use-package syslog-mode
  :after (hide-lines ov)
  :straight (syslog-mode :type git :host github :repo "vapniks/syslog-mode"))

;; Keep folding between sessions.
(use-package persistent-overlays)
(add-hook 'prog-mode-hook (lambda ()
                            (persistent-overlays-minor-mode 1)))
(add-hook 'find-filehook (lambda ()
                           (persistent-overlays-minor-mode 1)))

(straight-register-package
 '(org-books :type git :host github :repo "matthuszagh/org-books"))
(use-package org-books
  :after (org enlive org s helm helm-org dash)
  :config
  (setq org-books-file org-books-file))

(defun bookmark-relocate-noninteractive (bookmark-name newloc)
  "Relocate BOOKMARK-NAME to another file, NEWLOC.

This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it.

This is a modified version of the `bookmark-relocate' function
provided by `bookmark.el'.  I've modified it to be called
non-interactively."
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (bookmark-set-filename bookmark-name newloc)
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  (if (bookmark-time-to-save-p)
      (bookmark-save))
  (bookmark-bmenu-surreptitiously-rebuild-list))

(defun move-book (src dst)
  "Move library book and update the bookmarks file.

SRC is the file to move and DST is the destination directory.
You will be prompted to confirm the filename later."
  (interactive
   (list
    (read-file-name "src: " "/home/matt/library/")
    (read-directory-name "dst: " "/home/matt/library/")))
  (let* ((old-file (-take-last 1 (s-split "/" src)))
         (file (read-string "file name: " old-file))
         (bookmark-name-old (concat "PDF-LAST-VIEWED: " src))
         (bookmark-name-new (concat "PDF-LAST-VIEWED: " (concat dst file))))
    (progn (bookmark-relocate-noninteractive bookmark-name-old (concat dst file))
           (bookmark-rename bookmark-name-old bookmark-name-new)
           (start-process "mv" "*Messages*" "mv" src (concat dst file)))))

(defun read-book (file)
  "Open a PDF in the left buffer and the org book list in the right."
  (interactive
   (list
    (read-file-name "File: ")))
  (delete-other-windows)
  (split-window-right)
  (find-file file)
  (evil-window-right 1)
  (find-file org-books-file)
  (evil-window-left 1))

(use-package xwidgete)

;; (use-package elfeed
;;   :config
;;   (setq elfeed-feeds
;;         '("https://www.preposterousuniverse.com/blog/")))

(provide 'init)
;;; init.el ends here
