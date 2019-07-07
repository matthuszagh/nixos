;;; org-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def org
  :presetup
  (:layer straight
          (straight-use-package 'polymode)
          (straight-use-package 'poly-org))
  (:layer (straight helm)
          (straight-use-package 'helm-org-rifle))

  :setup
  (use-package org
    :demand t
    :hook
    ((org-mode . (lambda ()
                   (setq-local fill-column 70))))
    :config
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

    ;; ;; use habits
    ;; (add-to-list 'org-habit org-modules)
    ;; (setq org-habit-show-habits nil)

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
          '(("p" "pdf" entry (file "/home/matt/doc/notes/wiki.org")
             (concat "* %f\n"
                     ":PROPERTIES:\n"
                     ":Filepath: %a\n"
                     ":END:\n"
                     "** description\n"
                     "** outline [/]\n"
                     "%(mh/pdf-outline-to-org-headline \"%F\" 2 t)"))))
    (setq org-agenda-files (append
                            '("/home/matt/src/dotfiles/README.org")
                            (directory-files-recursively "/home/matt/doc/notes" "org$")))
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

    ;; make clocking efforts persistant across emacs sessions.
    ;; see [[info:org#Clocking%20Work%20Time][info:org#Clocking Work Time]]
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

    ;; set the column view format to include effort
    (setq org-columns-default-format (concat "%40ITEM(Task) "
                                             "%TODO %3PRIORITY "
                                             "%17Effort(Estimated Effort){:} "
                                             "%CLOCKSUM"))
    ;; keep the same column format in the agenda columns view
    (setq org-agenda-overriding-columns-format org-columns-default-format)

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

    ;; make contrib files visible
    ;; TODO modify this for nixpkgs
    (add-to-list 'load-path "~/.emacs.d/straight/repos/org/contrib/lisp" t))

  (use-package polymode)

  (use-package poly-org
    :after (polymode org)
    :config
    (add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode)))

  (use-package org-edna
    :config
    (org-edna-load))

  :postsetup
  (:layer pdf
          (defun mh/open-book-from-outline ()
            (interactive)
            (setq file (car (s-split "]" (car (last (s-split "file:" (org-entry-get (point) "Filepath")))))))
            (setq page (string-to-number (car (s-match "[0-9]+"
                                                       (car (s-match "([0-9]+)" (thing-at-point 'line t)))))))
            (find-file-other-window file)
            (pdf-view-goto-page page))

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

  (:layer helm
          (use-package helm-org-rifle
            :config
            (setq helm-org-rifle-show-path t)
            (setq helm-org-rifle-test-against-path t)))

  (:layer (helm modal-interaction)
          (general-def mh/prefix-search-map
            "h" 'helm-org-in-buffer-headings))

  (:layer modal-interaction
          ;; local org mode commands
          (localleader :keymaps 'org-mode-map
            "T" 'org-babel-tangle
            "l" 'org-insert-link
            ":" 'org-set-tags-command
            "i" 'org-id-get-create
            "c" 'org-columns
            "e" 'org-set-effort
            "a" 'org-archive-subtree)

          ;; org agenda keys
          ;; override org agenda keys and add back the ones you want
          ;; (setq org-agenda-mode-map (make-composed-keymap general-override-mode-map))
          (evil-set-initial-state 'org-agenda-mode 'normal)
          ;; (general-def 'org-agenda-mode-map
          ;;   "SPC" nil
          ;;   "j" nil
          ;;   "k" nil)
          (localleader :keymaps 'org-agenda-mode-map
            "c" 'org-agenda-columns
            "q" 'org-agenda-quit
            "i" 'org-agenda-clock-in
            "o" 'org-agenda-clock-out)

          ;; global org keys
          (general-define-key
           :keymaps 'mh/prefix-map
           :prefix "o"
           :prefix-command 'mh/command-org-prefix
           :prefix-map 'mh/prefix-org-map
           "c" 'org-capture
           "a" 'org-agenda
           "l" 'org-store-link
           "o" 'org-clock-out))

  (:layer internet
          (use-package org-eww)))

;;; org-layer.el ends here
