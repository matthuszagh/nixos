;; init.el --- Summary: -*-no-byte-compile: t; -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General configurations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-prefer-newer t)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Use the straight package manager instead of package.el.

;; We use a straight-maintained mirror, which fixes an issue that makes tex-sites.el unavailable to
;; AUCTeX.
(setq straight-recipes-gnu-elpa-use-mirror t)

;; Retreive straight if we don't have it.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq user-full-name "Matt Huszagh"
      user-mail-address "huszaghmatt@gmail.com")

;; Make system library paths available for tagging.
(setenv "GTAGSLIBPATH" "/home/matt/.gtags/")

;;; gnus
(setq gnus-init-file "~/.emacs.d/.gnus.el")
(setq gnus-use-full-window nil)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 1GB
(setq large-file-warning-threshold 1000000000)

;; Set image thumbnail size in image-dired
(setq image-dired-thumb-size 1000)

;; Human-readable sizes in dired mode.
(setq dired-listing-switches "-alh")

;; Load dired-x when in dired mode.
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; Disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; turn off auto-save
(setq auto-save-default nil)

;; Disable menu-bar
(menu-bar-mode -1)

(blink-cursor-mode -1)
;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Increase splitting threshold so that new buffers don't split existing ones.
(setq split-height-threshold 160)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)

(setq scroll-margin 0
      scroll-conservatively 0
      scroll-preserve-screen-position nil)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(which-function-mode 1)
(setq mode-line-format
      (quote
       ("%e"
        (:eval
         (window-numbering-get-number-string))
        mode-line-front-space mode-line-mule-info
        mode-line-client mode-line-modified mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification " " mode-line-position
        evil-mode-line-tag
        (vc-mode vc-mode)
        "  " mode-name mode-line-misc-info mode-line-end-spaces)))

;; Align comments
(defun align-comments (beginning end)
  "Align comments within marked region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)"
                                        (regexp-quote comment-start)))))

;; Hide the scroll bar
(scroll-bar-mode -1)
;; Ensure that scrollbar is also disabled for new frames.
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; Enable line numbers on the LHS
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(add-hook 'plain-TeX-mode-hook 'display-line-numbers-mode)

(defun display-line-numbers-on-files ()
  "Enable line numbers for most files, except some."
  (unless (eq major-mode 'pdf-view-mode)
    'display-line-numbers-mode))

(add-hook 'find-file-hook 'display-line-numbers-on-files)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
(add-hook 'before-save-hook
          (lambda ()
            (when (not (derived-mode-p 'ein:notebook-multilang-mode))
              (delete-trailing-whitespace))))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8) ;; but maintain correct appearance

;; Use the PgUp and PgDn keys to cycle between buffers.
(global-set-key (kbd "<next>") 'next-buffer)
(global-set-key (kbd "<prior>") 'previous-buffer)

;; Change tabs to spaces when copying text. This is useful for copying to external applications.
(defun copy-untabify (start end)
  "Copy the region.
Tabs at the beginning of each line are replaced by the same
amount of spaces."
  (interactive "r")
  (kill-new
   (replace-regexp-in-string
    "^\t+"
    (lambda (substring)
      (make-string (* tab-width (length substring)) ?\s))
    (buffer-substring start end))))

;; Open a file with sudo
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun copy-file-path ()
  "Copy the absolute file path of the current file to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun copy-file-name ()
  "Copy the file name of the current file to the clipboard."
  (interactive)
  (let ((buffername (buffer-name)))
    (when buffername
      (with-temp-buffer
        (insert buffername)
        (clipboard-kill-region (point-min) (point-max)))
      (message buffername))))

;; Indent entire buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "C-M-\\") 'indent-buffer)

;; C-c o switches to minibuffer.
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key (kbd "C-c o") 'switch-to-minibuffer)

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

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))))

;; Specify buffers that should appear in the same window
(add-to-list 'same-window-buffer-names "*Proced*")
(add-to-list 'same-window-buffer-names "*SQL*")
(add-to-list 'same-window-buffer-names "*Inferior Octave*")
(add-to-list 'same-window-buffer-names "*Faces*")
(add-to-list 'same-window-regexps ".*ein.*")

;; Newline at end of file
(setq require-final-newline t)

;; Auto-wrap at 100 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 100)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Amount of time before refontifying text.
;;(jit-lock-context-time 0.1)

;; Settings for searching
;; case insensitive searches by default
(setq-default case-fold-search t)
;; highlight matches when searching
(setq-default search-highlight t)

;; Insert parentheses, quotes, etc. in pairs.
(electric-pair-mode 1)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; Comment or uncomment the region
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Global variables
(setq org-books-file "~/library/book-list.org")

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

;; use eww as default browser
(setq browse-url-browser-function 'eww-browse-url)
;; use monospaced rather than proportional fonts
(setq shr-use-fonts nil)

;;;; Appearance

(use-package sourcerer-theme
  :config
  (load-theme 'sourcerer))

(defvar my-font-size 10)
(setq default-frame-alist `((cursor-color . "#c2c2b0")
                            (fullscreen . maximized) ; start emacsclient maximized
                            (font . ,(concat "Source Code Pro-" (number-to-string my-font-size)))
                            (hl-line-face . "gray16")))

(use-package smart-mode-line
  :init
  :after sourcerer-theme
  :config
  (setq sml/theme 'respectful)
  (setq sml/name-width 40)
  (sml/setup))

(setq outline-minor-mode t)

;;;; Built-in packages.

(use-package erc
  :hook (erc-mode . (lambda ()
                      (setq-local fill-column nil)))
  :config
  (setq erc-prompt-for-password nil)
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#guix" "#coreboot"))))

;; wrap text at 70 in eww
(setq shr-width 70)

(global-set-key (kbd "C-c m") 'eshell)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

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

;; Maintains a menu of recently opened files.
(use-package recentf
  :functions recentf-mode
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode t))

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package cl-lib)

(use-package multi-line
  :config
  (global-set-key (kbd "C-c d") 'multi-line))

(use-package proced
  :after (symon)
  :bind (("<f9>" . (lambda () (interactive)
                     (proced)
                     (command-execute 'symon-mode)))))

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :hook (python-mode . (lambda ()
                         (setq tab-width 4)))
  :config
  (setq-default python-indent 4)
  (setq-default python-indent-offset 4)
  (setq-default pdb-command-name "python -m pdb")
  ;; Use ipython as the python shell interpreter.
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

(use-package gdb-mi
  :hook (gud-mode . (lambda ()
                      (set (make-local-variable 'company-backends) '(company-capf))))
  :config
  ;; Don't pop io buffer to top
  (setq gdb-display-io-nopopup t)
  ;; Show source buffer at startup
  (setq gdb-show-main t)
  ;; Force gdb-mi to not dedicate any windows. Dedicated windows prevent switching it for another window.
  (advice-add 'gdb-display-buffer
	      :around (lambda (orig-fun &rest r)
			(let ((window (apply orig-fun r)))
			  (set-window-dedicated-p window nil)
			  window)))

  (advice-add 'gdb-set-window-buffer
	      :around (lambda (orig-fun name &optional ignore-dedicated window)
			(funcall orig-fun name ignore-dedicated window)
			(set-window-dedicated-p window nil))))

;; Mode for editing C and related languages such as C++.
(use-package cc-mode
  :after (company company-c-headers)
  :mode (("\\.c\\'" . c-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.tpp\\'" . c++-mode))
  :bind (:map c-mode-base-map
	      ("C-c C-c" . (lambda (cmd)
			     (interactive
			      (list
			       (compilation-read-command compile-command)))
			     (compile cmd t)))
              ("C-c C-k" . kill-compilation)
              ("RET" . newline-and-indent)
              ("C-c d" . 'manual-entry))
  :hook ((c-mode-common . (lambda ()
                            (add-hook 'before-save-hook 'clang-format-buffer nil t)))
         (c-mode-common . (lambda ()
                            (set (make-local-variable 'company-backends)
                                 (list '(company-c-headers company-rtags company-files))))))
  :config
  (setq tab-width 8)
  (setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "linux"))))

;; Semantic provides better code completion than many other frameworks. It does this by using a
;; realtime source code parser. It can therefore provide contextual code completion. For instance,
;; if you include a header file, it will parse that header and customize the completions based on
;; what's available in that header. The obvious downside is that it can create a lag after a header
;; is included. If this becomes unbearable, consider disabling it.
(use-package semantic
  :config
  ;; Cache parses for faster future access.
  (global-semanticdb-minor-mode 1)
  ;; Check if buffer is outdated when user is idle. If so, reparse.
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1)
  ;; Additional include paths.
  (semantic-add-system-include "/usr/include/boost" 'c++-mode)
  (semantic-add-system-include linux-header-path))

(use-package man
  :config
  (setq Man-notify-method (quote pushy)))

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(use-package seq)

(use-package org
  :init
  (setq book-file "/home/matt/library/book-list.org")
  (defun open-book-from-outline ()
    (interactive)
    (setq file (car (s-split "]" (car (last (s-split "file:" (org-entry-get (point) "Filepath")))))))
    (setq page (string-to-number (car (s-match "[0-9]+"
                                               (car (s-match "([0-9]+)" (thing-at-point 'line t)))))))
    (find-file-other-window file)
    (pdf-view-goto-page page))
  :hook (org-mode . (lambda ()
                      (if (equal buffer-file-name book-file)
                          (add-hook 'after-save-hook 'org-html-export-to-html nil t))))
  :bind (:map org-mode-map
              ("C-<return>" . open-book-from-outline))
  :config
  (setq org-log-done 'time
        org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
        org-todo-keyword-faces '(("INPROGRESS" . (:foreground "#cc8800"))))
  (setq org-directory (concat user-emacs-directory "org"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-capture-templates
        '(("b" "Book" entry (file book-file)
           "* %^{TITLE}\n\
:PROPERTIES:\n\
:Author:\n\
:Edition:\n\
:Pub-Year:\n\
:Filepath:\n\
:Goodreads-Ranking:\n\
:Goodreads-#-Reviews:\n\
:Amazon-Ranking:\n\
:Amazon-#-Reviews:\n\
:Goodreads-URL:\n\
:Amazon-URL:\n\
:OpenLibrary-URL:\n\
:ISBN:\n\
:END:\n%?" :empty-lines 1)))
  (setq org-agenda-files (list org-directory book-file))
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
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

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

;; sql
;; Silence compiler warnings
(use-package sql
  ;; Prevent auto newlines after a certain number of characters.
  :hook ((sql-interactive-mode . (lambda ()
                                   (set-fill-column 1000)))
         (sql-interactive-mode . my-sql-interactive-mode-hook))
  :config
  (defvar sql-product)
  (defvar sql-prompt-regexp)
  (defvar sql-prompt-cont-regexp)
  (defun my-sql-comint-preoutput-filter (output)
    "Filter prompts out of SQL query output.
Runs after `sql-interactive-remove-continuation-prompt' in
`comint-preoutput-filter-functions'."
    ;; If the entire output is simply the main prompt, return that.
    ;; (i.e. When simply typing RET at the sqli prompt.)
    (if (string-match (concat "\\`\\(" sql-prompt-regexp "\\)\\'") output)
        output
      ;; Otherwise filter all leading prompts from the output.
      ;; Store the buffer-local prompt patterns before changing buffers.
      (let ((main-prompt sql-prompt-regexp)
            (any-prompt comint-prompt-regexp) ;; see `sql-interactive-mode'
            (prefix-newline nil))
        (with-temp-buffer
          (insert output)
          (goto-char (point-min))
          (when (looking-at main-prompt)
            (setq prefix-newline t))
          (while (looking-at any-prompt)
            (replace-match ""))
          ;; Prepend a newline to the output, if necessary.
          (when prefix-newline
            (goto-char (point-min))
            (unless (looking-at "\n")
              (insert "\n")))
          ;; Return the filtered output.
          (buffer-substring-no-properties (point-min) (point-max))))))

  (defadvice sql-send-string (before my-prefix-newline-to-sql-string)
    "Force all `sql-send-*' commands to include an initial newline.

This is a trivial solution to single-line queries tripping up my
custom output filter.  (See `my-sql-comint-preoutput-filter'.)"
    (ad-set-arg 0 (concat "\n" (ad-get-arg 0))))
  (ad-activate 'sql-send-string)
  (defun my-sql-interactive-mode-hook ()
    "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
    (when (eq sql-product 'postgres)
      ;; Allow symbol chars in database names in prompt.
      ;; Default postgres pattern was: "^\\w*=[#>] " (see `sql-product-alist').
      (setq sql-prompt-regexp "^\\(?:\\sw\\|\\s_\\)*=[#>] ")
      ;; Ditto for continuation prompt: "^\\w*[-(][#>] "
      (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(][#>] "))

    ;; Deal with inline prompts in query output.
    ;; Runs after `sql-interactive-remove-continuation-prompt'.
    (add-hook 'comint-preoutput-filter-functions
              'my-sql-comint-preoutput-filter :append :local)))

;; term has 2 submodes: term-line-mode and term-char-mode. term-char-mode behaves like a normal
;; terminal whereas term-line-mode allows you to treat the terminal like a traditional
;; buffer. Keybindings for term-line-mode can be set in term-line-map whereas keybindings for
;; term-char-mode are set in term-raw-map. term-raw-escape-map is used to set keybindings for "C-c"
;; escaped commands. When setting escaped keys, omit the escaping "C-c".
(use-package term
  :after (helm evil)
  :config
  (defun expose-global-binding-in-term (binding)
    (define-key term-raw-map binding
      (lookup-key (current-global-map) binding)))
  (expose-global-binding-in-term (kbd "C-x"))
  (define-key term-raw-map (kbd "M-x") 'helm-M-x)
  (setq term-scroll-to-bottom-on-output t)
  (setq term-scroll-show-maximum-output t)
  ;; Don't limit term size
  (setq term-buffer-maximum-size 0))

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

(use-package image-dired)

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

(use-package bookmark)
;;;; Third-party packages.

(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))

(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-/" . nil))
  :config
  (global-undo-tree-mode))

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

(use-package pdf-tools
  :mode "\\.pdf\\'"
  :bind (("<next>" . next-buffer)
         ("<prior>" . previous-buffer)
         :map pdf-view-mode-map
         ("j" . pdf-view-next-line-or-next-page)
         ("k" . pdf-view-previous-line-or-previous-page))
  :config
  (pdf-tools-install))

(add-hook 'kill-buffer-hook 'pdf-set-last-viewed-bookmark)
(add-hook 'kill-emacs-hook #'pdf-set-all-last-viewed-bookmarks)
(add-hook 'pdf-view-mode-hook 'pdf-jump-last-viewed-bookmark)

(setq-default pdf-view-display-size 'fit-page)

(use-package a)

(use-package evil
  :init
  (setq evil-want-integration t) ; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; Evil sets keys in an unusual way and so we must use the define key directive instead of the
  ;; typical :bind.
  ;; Evil binds M-. which overrides the behavior of counsel-etags find tag.
  ;; This only seems to be an issue in normal mode with this keybinding.
  ;; If others are issues, perform similar actions with them.
  (define-key evil-normal-state-map (kbd "M-.") 'rtags-find-symbol-at-point)
  ;; Keep window navigation keys in evil Emacs state map.
  (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
  (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
  (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
  (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)
  (define-key evil-emacs-state-map (kbd "C-w H") 'evil-window-move-far-left)
  (define-key evil-emacs-state-map (kbd "C-w J") 'evil-window-move-very-bottom)
  (define-key evil-emacs-state-map (kbd "C-w K") 'evil-window-move-very-top)
  (define-key evil-emacs-state-map (kbd "C-w L") 'evil-window-move-far-right)
  ;; Alse keep it in evil insert state. This interferes with the normal vim keybinding of C-w. If
  ;; you want that back, delete these next lines.
  (define-key evil-insert-state-map (kbd "C-w") nil)
  (define-key evil-insert-state-map (kbd "C-w h") 'evil-window-left)
  (define-key evil-insert-state-map (kbd "C-w j") 'evil-window-down)
  (define-key evil-insert-state-map (kbd "C-w k") 'evil-window-up)
  (define-key evil-insert-state-map (kbd "C-w l") 'evil-window-right)
  (define-key evil-insert-state-map (kbd "C-w H") 'evil-window-move-far-left)
  (define-key evil-insert-state-map (kbd "C-w J") 'evil-window-move-very-bottom)
  (define-key evil-insert-state-map (kbd "C-w K") 'evil-window-move-very-top)
  (define-key evil-insert-state-map (kbd "C-w L") 'evil-window-move-far-right)
  ;; Unbind toggling Emacs state. I don't need this and would rather use it for term-mode.
  ;; If this functionality needs to be performed it can be done so with 'evil-emacs-state and
  ;; 'evil-exit-emacs-state.
  (define-key evil-emacs-state-map (kbd "C-z") nil))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)
  ;; Jump to the bottom of the window when entering insert mode in terminal.
  (evil-collection-define-key 'normal 'term-mode-map (kbd "i")
    (lambda ()
      (interactive)
      (progn (evil-insert-state)
             (term-show-maximum-output))))

  ;; term-mode configuration
  (evil-collection-define-key 'insert 'term-raw-map (kbd "C-c C-y") 'term-paste)
  (evil-collection-define-key 'insert 'term-raw-map (kbd "C-w") nil)

  ;; comint-mode configuration
  ;;
  ;; Same behavior for comint modes. Prevent this when in the middle of the line at the command
  ;; line. This allows evil navigation to edit the current command. I'd like this for term-mode too,
  ;; but it's much trickier (see emacs tex file).
  (evil-collection-define-key 'normal 'comint-mode-map (kbd "i")
    (lambda ()
      (interactive)
      (if (eq (line-number-at-pos)
	      (+ (evil-count-lines (point-min) (point-max)) 1))
	  (evil-insert-state)
	(progn (comint-show-maximum-output)
      	       (evil-insert-state)))))
  ;; Use C-p and C-n to cycle through inputs for consistency with term-mode.
  (evil-collection-define-key 'insert 'comint-mode-map
    (kbd "C-p") #'comint-previous-input
    (kbd "C-n") #'comint-next-input)

  ;; pdf-tools mode configuration
  (evil-collection-define-key 'normal 'pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (evil-collection-define-key 'normal 'pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)

  ;; doc-view-mode configuration
  ;;
  ;; Use the same page navigation in doc-view as in pdf-mode.
  (evil-collection-define-key 'normal 'doc-view-mode-map (kbd "j") 'doc-view-next-page)
  (evil-collection-define-key 'normal 'doc-view-mode-map (kbd "k") 'doc-view-previous-page)

  ;; org-mode configuration
  (evil-collection-define-key 'normal 'org-mode-map (kbd "<tab>") 'org-cycle)

  ;; proced-mode configuration
  (evil-collection-define-key 'normal 'proced-mode-map (kbd "q") (lambda () (interactive)
                                                                   (quit-window)
                                                                   (command-execute 'symon-mode)))
  ;; elpy and python
  (evil-collection-define-key 'normal 'python-mode-map (kbd "M-.") 'elpy-goto-definition)
  (evil-collection-define-key 'normal 'python-mode-map (kbd "C-M-.") 'elpy-goto-definition-other-window)

  ;; elisp
  (evil-collection-define-key 'normal 'emacs-lisp-mode-map (kbd "M-.") 'xref-find-definitions))

;; library for async/thread processing
(use-package async)

(use-package sage-shell-mode
  :hook (sage-shell-mode . (lambda ()
                             (set-fill-column 1000))))

(use-package pandoc-mode)

;; Looks for bugs in elisp files.
(use-package bug-hunter)

;; Startup profiler.
(use-package esup
  :config
  (autoload 'esup "esup" "Emacs Start Up Profiler." nil))

;; Display CPU/mem/etc usage.
(use-package symon
  :init
  ;; No delay when updating usage.
  (setq symon-delay 0)
  (setq symon-refresh-rate 1))

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

;; Auto update packages once a week
(use-package auto-package-update
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
            (lambda () (message "Automatically updating packages
            with auto-package-update..."))))

(use-package helm
  ;; Needed for "bind" keys to work on first loading.
  :demand t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("C-c h l" . helm-locate)
         ("C-c h g" . helm-google-suggest)
         ("M-:" . helm-eval-expression-with-eldoc)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ;; make tab work in terminal
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :init (setq helm-command-prefix-key "C-c h")
  :config
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t)
  (require 'helm-config)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        ;; search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp t
        ;; scroll 8 lines other window using M-<next>/M-<prior>
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 40)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  (setq helm-buffer-max-length 60))

;; Always use helm follow mode.
(setq-default helm-follow-mode-persistent t)
(with-eval-after-load 'helm-regexp
  (setq helm-source-occur
        (helm-make-source "Occur" 'helm-source-multi-occur
          :follow 1)))

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

(use-package projectile
  :after (helm)
  :bind ("C-c p" . helm-projectile)
  :config
  (projectile-global-mode)
  (setq helm-projectile-truncate-lines t)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :after (helm projectile cl-lib)
  :config
  (helm-projectile-on))

;; Describe Helm keybindings.
(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode))

;; Manage local packages with helm.
(use-package helm-system-packages
  :after (helm seq)
  :bind ("<f11>" . helm-system-packages))

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

(use-package helm-eww)

;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
(use-package window-numbering
  :config
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function window-numbering-mode "window-numbering.el"))
  (window-numbering-mode t))

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

;; Colors delimiters according to their depth.
(use-package rainbow-delimiters
  :functions rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key: when you pause on a keyboard shortcut it provides suggestions in a popup buffer
(use-package which-key
  :init
  :functions which-key-mode
  :config
  (which-key-mode))

;; A frontend for interfacing to external debuggers.
(use-package realgud
  :init
  (setenv "TERM" "dumb")
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;; Enhanced editing for python files.
(use-package elpy
  ;; :bind (([remap rtags-find-symbol-at-point] . elpy-goto-definition))
  :commands (elpy-enable)
  :hook (python-mode . elpy-mode)
  :config
  (elpy-enable)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package py-autopep8
  :hook (elpy-mode . py-autopep8-enable-on-save))

(use-package yapfify
  :hook (python-mode . yapf-mode))

(use-package clang-format
  :after (s)
  :init
  (defun get-clang-format-option (config-str field is-num)
    "Retrieve a config option from a clang-format config.

CONFIG-STR is a string containing the entire clang-format config.
FIELD is specific option, e.g. `IndentWidth'.  IS-NUM is a
boolean that should be set to 1 if the option is numeric,
otherwise assumed alphabetic."
    (if is-num
        (let ((primary-match (s-match (concat "^" field ":[ \t]*[0-9]+") config-str)))
          (if primary-match
              (string-to-number (car (s-match "[0-9]+" (car primary-match))))
            0))
      (let ((primary-match (s-match (concat "^" field ":[ \t]*[A-Za-z]+") config-str)))
        (if primary-match
            (car (s-match "[A-Za-z]+$" (car primary-match)))
          ""))))
  :bind (("C-c C-f" . clang-format-buffer))
  :hook (c-mode-common . (lambda ()
                           (let* ((clang-format-config
                                   (shell-command-to-string "clang-format -dump-config"))
                                  (c-offset (get-clang-format-option clang-format-config "IndentWidth" t))
                                  (tabs-str (get-clang-format-option clang-format-config "UseTab" nil))
                                  (indent-braces (get-clang-format-option
                                                  clang-format-config "IndentBraces" nil))
                                  (base-style
                                   (get-clang-format-option clang-format-config "BasedOnStyle" nil)))
                             (progn
                               (if (> c-offset 0)
                                   (setq-local c-basic-offset c-offset)
                                 (if (not (equal "" base-style))
                                     (cond ((or (equal "LLVM" base-style)
                                                (equal "Google" base-style)
                                                (equal "Chromium" base-style)
                                                (equal "Mozilla" base-style)
                                                (equal "GNU" base-style))
                                            (setq-local c-basic-offset 2))
                                           ((equal "WebKit" base-style)
                                            (setq-local c-basic-offset 4)))))
                               (if (not (equal "" tabs-str))
                                   (if (not (string-equal "Never" tabs-str))
                                       (setq-local indent-tabs-mode t)
                                     (setq-local indent-tabs-mode nil))
                                 (if (not (equal "" base-style))
                                     (cond ((or (equal "LLVM" base-style)
                                                (equal "Google" base-style)
                                                (equal "Chromium" base-style)
                                                (equal "Mozilla" base-style)
                                                (equal "WebKit" base-style)
                                                (equal "GNU" base-style))
                                            (setq-local indent-tabs-mode nil)))))
                               (if (not (equal "" indent-braces))
                                   (if (equal "true" indent-braces)
                                       (c-set-style "gnu")
                                     (c-set-style "linux"))
                                 (if (not (equal "" base-style))
                                     (cond ((or (equal "LLVM" base-style)
                                                (equal "Google" base-style)
                                                (equal "Chromium" base-style)
                                                (equal "Mozilla" base-style)
                                                (equal "WebKit" base-style))
                                            (c-set-style "linux"))
                                           ((equal "GNU" base-style)
                                            (c-set-style "gnu"))))))))))

;; Provides better C++ code highlighting.
(use-package modern-cpp-font-lock
  :functions modern-c++-font-lock-global-mode
  :config
  (modern-c++-font-lock-global-mode t))

(use-package verilog-mode
  :mode "\\.[st]*v[hp]*\\'"
  :bind (:map verilog-mode-map
              ("C-c C-f" . indent-buffer))
  :config
  ;; Turns off automatic newline after typing a semicolon, which is annoying.
  (setq verilog-auto-newline nil
        verilog-indent-level 8
        verilog-indent-level-declaration 8
        verilog-indent-level-behavioral 8
        verilog-indent-level-module 8
        verilog-case-indent 0
        indent-tabs-mode t
        verilog-auto-delete-trailing-whitespace t))

;; Allows better interaction with ElasticSearch.
(use-package es-mode
  :mode "\\.es$")

;; Code completions.
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
  ;; Zero delay when pressing tab
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
  :after (python company)
  :config
  (jedi:install-server))

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

;; Flycheck for python.
(use-package flycheck-pyflakes
  :after python)

;; Clang static analyzer
(use-package flycheck-clang-analyzer
  :after flycheck
  :functions flycheck-clang-analyzer-setup
  :init
  :config (flycheck-clang-analyzer-setup))

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

(use-package elisp-slime-nav)

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

;; git integration.
(use-package transient
  :straight (transient :type git :host github :repo "magit/transient")
  :config
  ;; Only display magit popup at the bottom of the current window rather than the entire frame.
  (setq transient-display-buffer-action '(display-buffer-below-selected)))

(use-package magit
  :straight (magit :type git :host github :repo "magit/magit")
  :after (async ghub dash git-commit with-editor transient)
  :hook (magit-mode . (lambda ()
                        (setq whitespace-mode -1)))
  :commands (magit-checkout))

(global-set-key (kbd "C-x g") 'magit-status)
;; Set fill-column to 50 in commit message buffer.
(add-hook 'git-commit-setup-hook (lambda ()
                                   (set-fill-column 50)))

(use-package evil-magit
  :after (magit transient))

;; Displays small signals to the left of line numbers in git repos to indicate diffs since the last
;; commit. A purple '=' means modified, a red '-' means deleted and a green '+' means added.
(use-package git-gutter
  :functions global-git-gutter-mode
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; Set the foreground color of modified lines to something obvious
  (set-face-foreground 'git-gutter:modified "purple"))

(use-package git-timemachine
  :bind* ("<f10>" . git-timemachine)
  :config
  ;; Make git-timemachine work with evil.
  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))

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

(global-set-key (kbd "<f5>") 'deadgrep)

;; Easy radix conversion
(use-package 0xc)

;; Query/browse stack exchange sites.
(use-package sx
  :config
  (bind-keys :prefix "C-c C-s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

;; Query REST sites.
(use-package restclient)

(use-package company-restclient
  :after (company cl-lib restclient know-your-http-well)
  :hook (restclient-mode . (lambda ()
                             (set (make-local-variable 'company-backends)
                                  (list
                                   (cons 'company-restclient default-company-backends))))))

;; auctex
(defun insert-frac ()
  "Insert '\frac{}{}' and position point before the first right brace."
  (interactive)
  (progn
    (insert "\\frac{}{}")
    (backward-char)
    (backward-char)
    (backward-char)))

(defun insert-text ()
  "Insert '\text{}' and position point inside the brace."
  (interactive)
  (progn
    (insert "\\text{}")
    (backward-char)))

(defun insert-math-subscript ()
  "Insert '_{\text{}}' and cursor to point inside middle brace."
  (interactive)
  (progn
    (insert "_{\\text{}}")
    (backward-char)
    (backward-char)))

(defun insert-left-delimiter ()
  "Insert '\left('."
  (interactive)
  (progn
    (insert "\\left(")))

(defun insert-right-delimiter ()
  "Insert '\right)'."
  (interactive)
  (progn
    (insert "\\right)")))

(defun latex-indent ()
  "Run latexindent on the current buffer."
  (interactive)
  (with-no-warnings
    (shell-command (concat "latexindent " (buffer-file-name) " > " (buffer-file-name) ".tmp && mv "
        		   (buffer-file-name) ".tmp " (buffer-file-name)))))

(defun add-auctex-keys ()
  ;; Make C-i distinguishable from tab.
  (define-key input-decode-map "\C-i" [C-i])
  (local-set-key (kbd "C-c <C-i> f") 'insert-frac)
  (local-set-key (kbd "C-c <C-i> t") 'insert-text)
  (local-set-key (kbd "C-c <C-i> s") 'insert-math-subscript)
  (local-set-key (kbd "C-c <C-i> l") 'insert-left-delimiter)
  (local-set-key (kbd "C-c <C-i> r") 'insert-right-delimiter)
  (local-set-key (kbd "C-c C-f") 'indent-buffer))

(add-hook 'LaTeX-mode-hook '(lambda ()
			      (local-set-key (kbd "C-c C-f") 'latex-indent)))

(add-hook 'LaTeX-mode-hook '(lambda () (interactive)
                              (if (null (TeX-PDF-mode))
                                  (command-execute 'TeX-PDF-mode))))

(add-hook 'plain-TeX-mode-hook '(lambda () (interactive)
                                  (if (TeX-PDF-mode)
			              (command-execute 'TeX-PDF-mode))))
(use-package tex-site
  :straight auctex
  ;; When we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . auto-fill-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . flyspell-buffer)
         (LaTeX-mode . (lambda ()
                         (TeX-fold-mode 1)
			 (add-hook 'find-file-hook 'TeX-fold-buffer t t)
                         (add-hook 'after-save-hook 'TeX-fold-buffer nil t)))
         (LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . add-auctex-keys)
         (LaTeX-mode . LaTeX-math-mode)
         (plain-TeX-mode . (lambda ()
                             (setq flycheck-disabled-checkers '(tex-chktex))))
         (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
         ;; Allows code folding. This is the same functionality that org mode uses.
         (LaTeX-mode . outline-minor-mode)
         ;; configure backend for tex source files.
         (LaTeX-mode . (lambda ()
                         (set (make-local-variable 'company-backends)
                              (list
                               (cons 'company-auctex-symbols
                                     (cons 'company-auctex-macros
                                           (cons 'company-auctex-environments
                                                 (cons 'company-auctex-bibs
                                                       (cons 'company-auctex-labels
                                                             (cons 'company-reftex
                                                                   (cons 'company-math default-company-backends))))))))))))
  :config
  ;; Use latex-mode by default when it is unclear whether plain-tex-mode or latex-mode should be
  ;; used.
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-master nil
                TeX-source-correlate-start-server t
                reftex-plug-into-AUCTeX t)
  (eval-after-load "tex-fold"
    '(add-to-list 'TeX-fold-macro-spec-list '("{2}" ("href"))))
  (eval-after-load "tex-fold"
    '(add-to-list 'TeX-fold-macro-spec-list '("{1}" ("hyperref"))))
  (eval-after-load "tex-fold"
    '(add-to-list 'TeX-fold-macro-spec-list '("{2}" ("mintinline"))))
  (eval-after-load "tex-fold"
    '(add-to-list 'TeX-fold-macro-spec-list '("{2}" ("hyperlink"))))
  ;; Disable default syntax highlighting in certain LaTeX environments.  This prevents certain
  ;; special characters from causing issues in those environments.  For instance, $ and _ See:
  ;; https://tex.stackexchange.com/questions/111289/how-to-make-auctex-ignore-syntax-highlighting-within-environment
  (setq LaTeX-verbatim-environments-local
        '("Verbatim" "lstlisting" "minted" "lstinline" "mintinline")))

;; Usability improvements for LaTeX.
(use-package latex-extra
  :after (auctex cl-lib))

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; Read EPUB.
(use-package nov
  :after (dash esxml)
  :mode "\\.epub\\'"
  :config
  (setq nov-text-width 80)
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "San Francisco Text"
                             :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Lookup Intel assembly mnemonics in Intel documentation.
(use-package x86-lookup
  :config
  (setq x86-lookup-pdf "~/Documents/library/Intel Software Developer’s Manual (2017).pdf"))

;; Easily switch window positions.
(use-package buffer-move
  :bind (("<C-S-right>" . buf-move-right)
	 ("<C-S-left>" . buf-move-left)
	 ("<C-S-up>" . buf-move-up)
	 ("<C-S-down>" . buf-move-down)))

;; Qt build system files.
(use-package qt-pro-mode
  :mode ("\\.pro\\'" "\\.pri\\'"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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

;; Finds common mistakes in English writing.
(use-package writegood-mode
  :functions writegood-mode
  :hook ((org-mode . writegood-mode)
         (git-commit-setup . writegood-mode)))

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

;; Multiple cursors to edit multiple lines at once.
(use-package multiple-cursors
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this))
  :config
  (setq mc/always-repeat-command t))

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

;; ipython notebooks in gui emacs
(defvar my:jupyter_location (executable-find "jupyter"))
(defvar my:jupyter_start_dir "/home/matt/.jupyter")
;; Only launch if the executable exists.
(if (and my:jupyter_location
         my:jupyter_start_dir)
    (use-package ein
      :bind (("<f2>" . ein:connect-to-notebook)
             :map ein:connect-mode-map
             ;; Preserve "M-," for jumping back after jumping to a definition, etc.
             ("M-," . nil))
      :commands (ein:jupyter-server-start)
      :hook (ein:notebook-multilang-mode . (lambda ()
                                             (display-line-numbers-mode -1)))
      :defer 5
      :config
      (require 'ein-notebook)
      (require 'ein-subpackages)
      ;; when editing the emacs.el file, we do not want to start a new
      ;; Jupyter server each time we save, so we only start a new Jupyter
      ;; server if there currently isn't one running.
      (defvar my-found-ein-server nil)
      (dolist (my-current-process (process-list))
        (when (string-match "EIN: Jupyter*" (process-name my-current-process))
          (setq my-found-ein-server t)))
      (when (not my-found-ein-server)
        (ein:jupyter-server-start my:jupyter_location my:jupyter_start_dir))))

(use-package cuda-mode
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

(use-package cmake-mode
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (set (make-local-variable 'company-backends)
                             (list
                              (cons 'company-cmake default-company-backends)))))
  :config
  (use-package cmake-font-lock
    :defer t
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                  1 font-lock-warning-face t)))))))

(use-package dockerfile-mode
  :mode ("Dockerfile"))

;; Allows the use of tmux in Emacs.
(use-package emamux
  :bind-keymap ("C-c e" . emamux:keymap)
  :config
  (setq emamux:completing-read-type 'helm))

(use-package multi-term
  :after (evil)
  :bind (("<C-next>" . multi-term-next)
         ("<C-prior>" . multi-term-prev)
         ("C-c t" . multi-term-next)
         ("<f1>" . multi-term))
  :config
  (require 'multi-term-ext)
  (setq multi-term-program "/bin/bash"))

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

(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-pair "$" "$" :wrap "C-$")
  (smartparens-global-mode t)
  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp))

(use-package evil-smartparens
  :after smartparens
  :hook
  (smartparens-enabled . evil-smartparens-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

;;;; Appearance

(use-package sourcerer-theme
  :config
  (load-theme 'sourcerer t))

(set-face-background 'hl-line "gray16")

(set-face-attribute 'cursor nil :foreground "#c2c2b0")
(setq default-frame-alist '((cursor-color . "#c2c2b0")))
;; Start emacsclient maximized.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Set font.
(add-to-list 'default-frame-alist '(font . "Source Code Pro-8"))

(use-package smart-mode-line
  :init
  :after sourcerer-theme
  :config
  (setq sml/theme 'respectful)
  (setq sml/name-width 40)
  (sml/setup))

(provide 'init)
;;; init.el ends here
