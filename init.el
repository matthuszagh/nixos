;;; initfile --- Summary:
;;; Commentary:
;; Emacs 26.1 and newer tested

;;; Code:

;; Suppress free variable warnings
(setq byte-compile-warnings '(not free-vars obsolete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration/Customization:
;; Defines global variables that are later used to customize and set
;; up packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify the ycmd server command and path to the ycmd directory *inside *the;
                                        ; cloned ycmd directory
;; (defvar my:ycmd-server-command '("python" "/home/matt/developer/software/ycmd/ycmd"))
;; (defvar my:ycmd-extra-conf-whitelist '("~/.ycm_extra_conf.py"))
;; (defvar my:ycmd-global-config "~/.ycm_extra_conf.py")

;; Specify the jupyter executable name, and the start dir of the server
(defvar my:jupyter_location (executable-find "jupyter"))
(defvar my:jupyter_start_dir "/home/matt")

;; Compilation command for C/C++
(defvar my:compile-command "make && ./")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; Extra plugins and config files are stored here
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start emacs server if not already running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; Use CUA to delete selections
(setq cua-mode t)
(setq cua-enable-cua-keys nil)
;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)
;; Set image thumbnail size in image-dired
(setq image-dired-thumb-size 500)
;; Disable the toolbar at the top since it's useless
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
;; Increase threshold for large file warning to 1GB
(setq large-file-warning-threshold 1000000000)
;;; gnus init file
(setq gnus-init-file "~/.emacs.d/.gnus.el")
(add-hook 'kill-emacs-hook
          (lambda ()
            (if (boundp 'gnus-buffers)
                (gnus-group-exit))))
;; Save buffers between sessions.
(desktop-save-mode 1)
;; Display function
(which-function-mode 1)
;; Display buffer size
(size-indication-mode 1)
;; Only load some buffers immediately, load the others lazily when Emacs is idle.
(setq desktop-restore-eager 50)
;; Ensure undo-tree is installed.
(require 'undo-tree)
;; Change tab key behavior to insert spaces instead
(setq-default indent-tabs-mode nil)
;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 4)
;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 4)
;; Improve PDF resolution in DocView
(require 'doc-view)
(setq doc-view-resolution 300)
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

;; Copy file fath to clipboard
(defun copy-file-path ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; Indent entire buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "C-M-\\") 'indent-buffer)

;; Keybindings for image mode
(add-hook 'image-mode-hook
          (lambda ()
            (local-set-key (kbd "C-\+") 'image-increase-size)
            ))
(add-hook 'image-mode-hook
          (lambda ()
            (local-set-key (kbd "C-\-") 'image-decrease-size)
            ))

;; Specify buffers that should appear in the same window
(add-to-list 'same-window-buffer-names "*Proced*")
(add-to-list 'same-window-buffer-names "*SQL*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pdf-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)
(pdf-tools-install)
(defun pdf-view--rotate (&optional counterclockwise-p page-p)
  "Rotate PDF 90 degrees.  Requires pdftk to work.\n
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

;; Revert PDF after TeX compilation has finished
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
(defun brds/pdf-set-last-viewed-bookmark ()
  (interactive)
  (when (eq major-mode 'pdf-view-mode)
    (bookmark-set (brds/pdf-generate-bookmark-name))))

(defun brds/pdf-jump-last-viewed-bookmark ()
  (bookmark-set "fake") ; this is new
  (when
      (brds/pdf-has-last-viewed-bookmark)
    (bookmark-jump (brds/pdf-generate-bookmark-name))))

(defun brds/pdf-has-last-viewed-bookmark ()
  (assoc
   (brds/pdf-generate-bookmark-name) bookmark-alist))

(defun brds/pdf-generate-bookmark-name ()
  (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

(defun brds/pdf-set-all-last-viewed-bookmarks ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (brds/pdf-set-last-viewed-bookmark))))

(add-hook 'kill-buffer-hook 'brds/pdf-set-last-viewed-bookmark)
(add-hook 'pdf-view-mode-hook 'brds/pdf-jump-last-viewed-bookmark)
(unless noninteractive  ; as `save-place-mode' does
  (add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  ;; Set some modes to default to Emacs keybindings.
  (dolist (mode '(git-rebase-mode
                  flycheck-error-list-mode
                  pdf-outline-buffer-mode
                  sx-question-mode
                  sx-question-list-mode
                  pdf-occur-buffer-mode
                  mpdel-nav-mode
                  slime-repl-mode
		  image-dired-thumbnail-mode
                  inferior-octave-mode
                  Custom-mode
		  eshell-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'term-char-mode 'emacs)
  (evil-set-initial-state 'term-line-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (evil-set-initial-state 'gud-mode 'emacs)
  (evil-set-initial-state 'sage-shell-mode 'emacs)
  (evil-set-initial-state 'message-mode 'emacs)
  (evil-set-initial-state 'sql-interactive-mode 'emacs)

  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point))))

  ;; Evil binds M-. which overrides the behavior of counsel-etags find tag.
  ;; This only seems to be an issue in normal mode with this keybinding.
  ;; If others are issues, perform similar actions with them.
  (define-key evil-normal-state-map (kbd "M-.") 'rtags-find-symbol-at-point)
  )

;; Unbind toggling Emacs state. I don't need this and would rather use it for term-mode.
;; If this functionality needs to be performed it can be done so with 'evil-emacs-state and
;; 'evil-exit-emacs-state.
(define-key evil-emacs-state-map (kbd "C-z") nil)


;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
(add-hook 'before-save-hook
          (lambda ()
            (when (not (derived-mode-p 'ein:notebook-multilang-mode))
              (delete-trailing-whitespace))))

;; Dim buffers other than the active one to more clearly show which is currently active.
;; (add-hook 'after-init-hook (lambda ()
;;   (when (fboundp 'auto-dim-other-buffers-mode)
;;     (auto-dim-other-buffers-mode t))))

;; Auto-wrap at 100 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 120)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))
;; Automatically revert buffers
(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Global Keyboard Shortcuts
(define-key input-decode-map "\C-i" [C-i])
;; Set help to C-?
(global-set-key (kbd "C-?") 'help-command)
;; Set mark paragraph to M-?
(global-set-key (kbd "M-?") 'mark-paragraph)
;; Set backspace to C-h
(global-set-key (kbd "C-h") 'delete-backward-char)
;; Set backspace word to M-h
(global-set-key (kbd "M-h") 'backward-kill-word)
;; Use meta+tab word completion
(global-set-key (kbd "M-TAB") 'dabbrev-expand)
;; Show buffer list in the current active buffer
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; Comment or uncomment the region
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(define-key undo-tree-map (kbd "C-/") nil)
;; Indent after a newline, if required by syntax of language
(global-set-key (kbd "C-m") 'newline-and-indent)
;; Shortcut for M-x multi-term
(global-set-key (kbd "C-c t") 'multi-term)
;; Find file in project
(global-set-key (kbd "C-x M-f") 'project-find-file)
;; Turn off blinking cursor.
(blink-cursor-mode 0)

;; C-c o switches to minibuffer.
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

;; Load the compile command
;; Use "C-c i" in a compilation buffer to allow user input.
(global-set-key (kbd "C-c C-c") 'compile)
(setq compilation-scroll-output 'next-error)
(setq compilation-skip-threshold 2)

(require 'cl-lib)
(defun endless/toggle-comint-compilation ()
  "Restart compilation with (or without) `comint-mode'."
  (interactive)
  (cl-callf (lambda (mode) (if (eq mode t) nil t))
      (elt compilation-arguments 1))
  (recompile))

(define-key compilation-mode-map (kbd "C-c i")
  #'endless/toggle-comint-compilation)
(define-key compilation-minor-mode-map (kbd "C-c i")
  #'endless/toggle-comint-compilation)
(define-key compilation-shell-minor-mode-map (kbd "C-c i")
  #'endless/toggle-comint-compilation)

;; rgrep
(global-set-key (kbd "<f7>") #'rgrep)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)

;; Disable the menu bar since we don't use it, especially not in the
;; terminal
;; (when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
;; (menu-bar-mode -1))
(menu-bar-mode -1)

;; Make environment variables available to Emacs.
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; ;; Highlight some keywords in prog-mode
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             ;; Highlighting in cmake-mode this way interferes with
;;             ;; cmake-font-lock, which is something I don't yet understand.
;;             (when (not (derived-mode-p 'cmake-mode))
;;               (font-lock-add-keywords
;;                nil
;;                '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
;;                   1 font-lock-warning-face t))))
;;             )
;;           )

;; Setup use-package
(eval-when-compile
  (require 'use-package))
(use-package bind-key
  :ensure t)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package
  :commands use-package-autoload-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable terminal emacs to copy and paste from system clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: this uses C-c before the usual C-w, M-w, and C-y
;; From: https://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
(defun my-copy-to-xclipboard(arg)
  (interactive "P")
  (cond
   ((not (use-region-p))
    (message "Nothing to yank to X-clipboard"))
   ((and (not (display-graphic-p))
         (/= 0 (shell-command-on-region
                (region-beginning) (region-end) "xsel -i -b")))
    (message "Error: Is program `xsel' installed?"))
   (t
    (when (display-graphic-p)
      (call-interactively 'clipboard-kill-ring-save))
    (message "Yanked region to X-clipboard")
    (when arg
      (kill-region  (region-beginning) (region-end)))
    (deactivate-mark))))

(defun my-cut-to-xclipboard()
  (interactive)
  (my-copy-to-xclipboard t))

(defun my-paste-from-xclipboard()
  (interactive)
  (if (display-graphic-p)
      (clipboard-yank)
    (insert (shell-command-to-string "xsel -o -b"))))

(global-set-key (kbd "C-c C-w") 'my-cut-to-xclipboard)
(global-set-key (kbd "C-c M-w") 'my-copy-to-xclipboard)
(global-set-key (kbd "C-c C-y") 'my-paste-from-xclipboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; async - library for async/thread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electric pair mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sage-shell-mode-hook
          (lambda ()
            (set-fill-column 1000)))
(global-set-key (kbd "<f6>") 'sage-shell:run-sage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proced
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f9>") 'proced)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer - better buffer list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      filename-and-process)))

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

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(defadvice ibuffer-update-title-and-summary (after remove-column-titles)
  (save-excursion
    (set-buffer "*Ibuffer*")
    (toggle-read-only 0)
    (goto-char 1)
    (search-forward "-\n" nil t)
    (delete-region 1 (point))
    (let ((window-min-height 1))
      ;; save a little screen estate
      (shrink-window-if-larger-than-buffer))
    (toggle-read-only)))

(ad-activate 'ibuffer-update-title-and-summary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symon - display CPU/mem/etc usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'symon)
(symon-mode)
(global-set-key (kbd "<f1> z") 'symon-mode)
;; (setq symon-delay 10)

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(require 'plantuml_helpers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime - package for common lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google-this - package to google selected text or text under cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(google-this-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s is used by ycmd, origami, etc and sometimes during Emacs
;; upgrades disappears so we try to install it on its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save ~/.emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete redefine))
      (setq byte-compile-warnings
            '(unresolved
              callargs
              ;; redefine
              obsolete
              noruntime
              cl-warnings
              interactive-only)))
    (byte-compile-file (expand-file-name file)))
  )

(add-hook
 'after-save-hook
 (function
  (lambda ()
    (if (string= (file-truename "~/.emacs.d/init.el")
                 (file-truename (buffer-file-name)))
        (byte-compile-init-files (file-truename "~/.emacs.d/init.el")))
    )
  )
 ;; #'load-file user-init-file
 )

;; Byte-compile again to ~/.emacs.d/init.elc if it is outdated
(if (file-newer-than-file-p
     (file-truename "~/.emacs.d/init.el")
     (file-truename "~/.emacs.d/init.elc"))
    (byte-compile-init-files "~/.emacs.d/init.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
            (lambda () (message "I will update packages now")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package ivy
;;   :ensure t
;;   :commands (ivy-mode)
;;   :config
;;   (require 'ivy)
;;   (ivy-mode t)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (setq ivy-wrap t)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   ;; Show #/total when scrolling buffers
;;   (setq ivy-count-format "%d/%d ")
;;   )

;; (use-package swiper
;;   :ensure t
;;   :bind (("C-s" . swiper)
;;          ("C-r" . swiper))
;;   )

;; (use-package counsel
;;   :ensure t
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-x C-f" . counsel-find-file)
;;          ("<f1> f" . counsel-describe-function)
;;          ("<f1> v" . counsel-describe-variable)
;;          ("<f1> l" . counsel-find-library)
;;          ("<f2> i" . counsel-info-lookup-symbol)
;;          ("<f2> u" . counsel-unicode-char)
;;          ("C-c g" . counsel-git-grep)
;;          ("C-c j" . counsel-git)
;;          ("C-c k" . counsel-ag)
;;          ("C-c r" . counsel-rg)
;;          ("C-x l" . counsel-locate)
;;          :map minibuffer-local-map
;;          ("C-r" . counsel-minibuffer-add)
;;          )
;;   :config
;;   (if (executable-find "rg")
;;       ;; use ripgrep instead of grep because it's way faster
;;       (setq counsel-grep-base-command
;;             "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
;;             counsel-rg-base-command
;;             "rg -i -M 120 --no-heading --line-number --color never %s ."
;;             )
;;     (warn "\nWARNING: Could not find the ripgrep executable. It "
;;           "is recommended you install ripgrep.")
;;     )
;;   )

;; ;; Use universal ctags to build the tags database for the project.
;; ;; When you first want to build a TAGS database run 'touch TAGS'
;; ;; in the root directory of your project.
;; (use-package counsel-etags
;;   :ensure t
;;   :init
;;   (eval-when-compile
;;     ;; Silence missing function warnings
;;     (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
;;     (declare-function counsel-etags-guess-program "counsel-etags.el")
;;     (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
;;   :bind (
;;          ("M-." . counsel-etags-find-tag-at-point)
;;          ("M-t" . counsel-etags-grep-symbol-at-point)
;;          ("M-s" . counsel-etags-find-tag))
;;   :config
;;   ;; Ignore files above 800kb
;;   (setq counsel-etags-max-file-size 800)
;;   ;; Ignore build directories for tagging
;;   (add-to-list 'counsel-etags-ignore-directories '"build*")
;;   (add-to-list 'counsel-etags-ignore-directories '".vscode")
;;   (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
;;   ;; Don't ask before rereading the TAGS files if they have changed
;;   (setq tags-revert-without-query t)
;;   ;; Don't warn when TAGS files are large
;;   (setq large-file-warning-threshold nil)
;;   ;; How many seconds to wait before rerunning tags for auto-update
;;   (setq counsel-etags-update-interval 180)
;;   ;; Set up auto-update
;;   (add-hook
;;    'prog-mode-hook
;;    (lambda () (add-hook 'after-save-hook
;;                         (lambda ()
;;                           (counsel-etags-virtual-update-tags))))
;;    )

;;   ;; The function provided by counsel-etags is broken (at least on Linux)
;;   ;; and doesn't correctly exclude directories, leading to an excessive
;;   ;; amount of incorrect tags. The issue seems to be that the trailing '/'
;;   ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
;;   ;; in that directory, only files in sub-directories of the dir set to be
;;   ;; ignore.
;;   (defun my-scan-dir (src-dir &optional force)
;;     "Create tags file from SRC-DIR. \
;;      If FORCE is t, the commmand is executed without \
;;      checking the timer."
;;     (let* ((find-pg (or
;;                      counsel-etags-find-program
;;                      (counsel-etags-guess-program "find")))
;;            (ctags-pg (or
;;                       counsel-etags-tags-program
;;                       (format "%s -e -L" (counsel-etags-guess-program
;;                                           "ctags"))))
;;            (default-directory src-dir)
;;            ;; run find&ctags to create TAGS
;;            (cmd (format
;;                  "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
;;                  find-pg
;;                  (mapconcat
;;                   (lambda (p)
;;                     (format "-iwholename \"*%s*\"" p))
;;                   counsel-etags-ignore-directories " -or ")
;;                  counsel-etags-max-file-size
;;                  (mapconcat (lambda (n)
;;                               (format "-not -name \"%s\"" n))
;;                             counsel-etags-ignore-filenames " ")
;;                  ctags-pg))
;;            (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
;;            (doit (or force (not (file-exists-p tags-file)))))
;;       ;; always update cli options
;;       (when doit
;;         (message "%s at %s" cmd default-directory)
;;         (shell-command cmd)
;;         (visit-tags-table tags-file t)
;;         )
;;       )
;;     )

;;   (setq counsel-etags-update-tags-backend
;;         (lambda ()
;;           (interactive)
;;           (let* ((tags-file (counsel-etags-locate-tags-file)))
;;             (when tags-file
;;               (my-scan-dir (file-name-directory tags-file) t)
;;               (run-hook-with-args
;;                'counsel-etags-after-update-tags-hook tags-file)
;;               (unless counsel-etags-quiet-when-updating-tags
;;                 (message "%s is updated!" tags-file))))
;;           )
;;         )
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
(setq helm-locate-fuzzy-match t)
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
;; (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
(use-package window-numbering
  :ensure t
  :config
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function window-numbering-mode "window-numbering.el"))
  (window-numbering-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit server to allow editing of things in Chrome with Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package edit-server
;;   :ensure t
;;   :config
;;   (progn
;;     (eval-when-compile
;;       ;; Silence missing function warnings
;;       (declare-function edit-server-start "edit-server-start.el"))
;;     (when (daemonp)
;;       (edit-server-start)
;;       )
;;     (add-hook 'edit-server-start-hook
;;               (lambda ()
;;                 (when (string-match "github.com" (buffer-name))
;;                   (markdown-mode))))
;;     )
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function beacon-mode "beacon.el"))
  :config
  (beacon-mode nil))

(beacon-mode 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function which-key-mode "which-key.el"))
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: always fast jump to char inside the current view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1))
  ;; Set keys for Dvorak mode instead of qwerty
  :init (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
                            ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
                            ?p ?y ?f ?g ?c ?r ?l
                            ?P ?Y ?F ?G ?C ?R ?L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zzz-to-char: replaces the built-in zap-to-char with avy-like
;;              replacement options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zzz-to-char
  :ensure t
  :bind ("M-z" . zzz-up-to-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RealGud - https://github.com/realgud/realgud
;; A rewrite of GUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package realgud
  :ensure t
  :init
  (setenv "TERM" "dumb")
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-indent 4)
(setq-default python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)))
(setq-default pdb-command-name "python -m pdb")
(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :after python
  :config
  (elpy-enable)
  )

(require 'elpy)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(use-package yapfify
  :ensure t
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-buffer))
  )

;; GDB configuration
(setq gdb-many-windows t)

;; Suppress warnings with reference to free variable.
;; Keep source line centered on screen
;; (defadvice gud-display-line (after gud-display-line-centered activate)
;;   "Center the line in the window"
;;   (when (and gud-overlay-arrow-position gdb-source-window)
;;     (with-selected-window gdb-source-window
;;                                         ; (marker-buffer gud-overlay-arrow-position)
;;       (save-restriction
;;         (with-no-warnings
;;           (goto-line (ad-get-arg 1)))
;;         (recenter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern C++ code highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modern-cpp-font-lock
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function modern-c++-font-lock-global-mode
                      "modern-cpp-font-lock.el"))
  :config
  (modern-c++-font-lock-global-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-default-style "linux")

(defun my-c-mode-common-hook ()
  (setq c-basic-offset 8
	tab-width 8
	indent-tabs-mode t))

(use-package cc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
  (define-key c-mode-base-map (kbd "C-c C-k") 'kill-compilation)
  (setq compile-command my:compile-command)
  ;; (use-package google-c-style
  ;;   :ensure t
  ;;   :config
  ;;   ;; This prevents the extra two spaces in a namespace that Emacs
  ;;   ;; otherwise wants to put... Gawd!
  ;;   ;; (add-hook 'c-mode-common-hook 'google-set-c-style)
  ;;   ;; Autoindent using google style guide
  ;;   ;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  ;;   )
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; We want to be able to see if there is a tab character vs a space.
;; global-whitespace-mode allows us to do just that.
;; Set whitespace mode to only show tabs, not newlines/spaces.
(use-package whitespace
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-whitespace-mode "whitespace.el"))
  :config
  ;; (setq whitespace-style '(tabs tab-mark))
  (setq whitespace-style nil)
  ;; Turn on whitespace mode globally.
  (global-whitespace-mode t)
  )

;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verilog mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package verilog-mode
  ;; :load-path "elisp/verilog-mode"
  :mode (("\\.[st]*v[hp]*\\'" . verilog-mode))
  :config
  ;; Turns off automatic newline after typing a semicolon, which is annoying.
  (setq verilog-auto-newline nil)
  (setq verilog-indent-level 8)
  (setq verilog-indent-level-declaration 8)
  (setq verilog-indent-level-behavioral 8)
  (setq verilog-indent-level-module 8)
  (setq verilog-case-indent 0)
  (setq verilog-auto-delete-trailing-whitespace t)
  )

(defun my-verilog-hook ()
  (local-set-key (kbd "C-c C-f") 'indent-buffer)
  (setq indent-tabs-mode t)
  )
(add-hook 'verilog-mode-hook 'my-verilog-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile - project-centered commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: ycmd (YouCompleteMeDaemon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Set up YouCompleteMe for emacs:
;; ;; https://github.com/Valloric/ycmd
;; ;; https://github.com/abingham/emacs-ycmd
;; (defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))
;; (if (not my:python-location)
;;     (message
;;      "Could not start YouCompleteMeDaemon because the python executable could
;; not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
;; appropriately in ~/.emacs.d/init.el.\n" (nth 0 my:ycmd-server-command)))
;; (if (not (file-directory-p (nth 1 my:ycmd-server-command)))
;;     (message "Could not YouCompleteMeDaemon because the specified directory does
;; not exist.\nSpecified directory is: '%s'
;; Please set my:ycmd-server-command appropriately in ~/.emacs.d/init.el.\n"
;;              (nth 1 my:ycmd-server-command)))
;; (if (and my:python-location
;;          (file-directory-p (nth 1 my:ycmd-server-command)))
;;     (use-package ycmd
;;       :ensure t
;;       :init
;;       (eval-when-compile
;;         ;; Silence missing function warnings
;;         (declare-function global-ycmd-mode "ycmd.el")
;; 	    (declare-function ycmd-mode "ycmd.el"))
;;       (add-hook 'after-init-hook #'global-ycmd-mode)
;;       :config
;;       (progn
;;         (set-variable 'ycmd-server-command my:ycmd-server-command)
;;         (set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
;;         (set-variable 'ycmd-global-config my:ycmd-global-config)
;;         (setq ycmd-force-semantic-completion t)
;;         (setq ycmd-request-message-level -1)
;;         (setq ycmd-url-show-status nil)
;;         (setq ycmd--log-enabled t)
;;         (use-package company-ycmd
;;           :ensure t
;;           :init
;;           (eval-when-compile
;;             ;; Silence missing function warnings
;;             (declare-function company-ycmd-setup "company-ycmd.el"))
;;           :config
;;           (company-ycmd-setup)
;;           )

;;         ;; (use-package flycheck-ycmd
;;         ;;   :ensure t
;;         ;;   :init
;;         ;;   (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
;;         ;;   )

;;         ;; Add displaying the function arguments in mini buffer using El Doc
;;         (require 'ycmd-eldoc)
;;         (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
;;         ;; disable ycmd in tramp mode when editing C++ and python files
;;         ;; (ycmd doesn't work with tramp)
;;         (add-hook 'c++-mode-hook
;;                   (unless (tramp-tramp-file-p (buffer-file-name (current-buffer)))
;;                     (ycmd-mode)))
;;         (add-hook 'c-mode-hook
;;                   (unless (tramp-tramp-file-p (buffer-file-name (current-buffer)))
;;                     (ycmd-mode)))
;;         (add-hook 'python-mode-hook
;;                   (unless (tramp-tramp-file-p (buffer-file-name (current-buffer)))
;;                     (ycmd-mode)))
;;         )
;;       )
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; es-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'es-mode)
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Disable company in specific major modes.
  (setq company-global-modes '(not web-mode plantuml-mode))
  ;; remove unused backends
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )

;; Setup loading company-jedi for python completion
;; This requines running jedi:install-server the first time
(use-package company-jedi
  :ensure t
  :after python
  :init
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck-ycmd
(use-package flycheck
  :ensure t
  :defer t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  :bind ("C-c l" . flycheck-list-errors)
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  ;; Verilog Verilator static analyzer
  (setq-default flycheck-verilog-verilator-executable "/usr/local/bin/verilator_bin")
  )
(use-package flycheck-pyflakes
  :ensure t
  :after python)

;; Clang static analyzer
(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :init
  (eval-when-compile
    (declare-function flycheck-clang-analyzer-setup "flycheck-clang-analyzer.el"))
  :config (flycheck-clang-analyzer-setup))

;; (with-eval-after-load 'flycheck
;;   (require 'flycheck-clang-analyzer)
;;   (flycheck-clang-analyzer-setup))

;; ;; Cpp Check
;; (require 'flymake-cppcheck)
;; (add-hook 'c-mode-hook 'flymake-cppcheck-load)
;; (add-hook 'c++-mode-hook 'flymake-cppcheck-load)

;; ;; Rtags
;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  ;; (setq-local flycheck-highlighting-mode nil)
  ;; (setq-local flycheck-check-syntax-automatically nil))
  )

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
  (setq rtags-use-helm t)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags))

(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

;; cmake ide
(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(flymake-cppcheck-enable "all")
 '(git-gutter:update-interval 5)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 50 50 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process))))
 '(jit-lock-context-time 0.1)
 '(line-number-mode nil)
 '(mode-line-format
   (quote
    ("%e"
     (:eval
      (window-numbering-get-number-string))
     mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
     (vc-mode vc-mode)
     "  " mode-name mode-line-misc-info mode-line-end-spaces)))
 '(package-selected-packages
   (quote
    (flycheck-rtags helm-rtags helm-projectile rtags helm-system-packages systemd ac-octave es-mode eterm-256color xterm-color helm-systemd helm magit yasnippet-snippets flycheck-plantuml flycheck-clang-analyzer flycheck-pyflakes flycheck debbugs exec-path-from-shell libmpdee ivy-mpdel mpdel auto-dim-other-buffers flycheck-verilator flycheck-verilog-verilator ov hide-lines flymake-cppcheck py-autopep8 djvu plantuml-mode etable el-get deadgrep 0xc ac-slime slime sx google-this company-restclient restclient sage-shell-mode auctex-latexmk nov nasm-mode x86-lookup buffer-move evil pdf-tools qt-pro-mode auto-complete-exuberant-ctags markdown-mode elpy realgud beacon wgrep use-package zzz-to-char yasnippet yapfify yaml-mode writegood-mode window-numbering which-key web-mode vlf test-simple swiper-helm string-inflection sourcerer-theme ripgrep rainbow-delimiters pyvenv powerline origami multiple-cursors modern-cpp-font-lock magit-gerrit loc-changes load-relative json-mode hungry-delete highlight-indentation google-c-style git-gutter flyspell-correct-ivy elscreen-multi-term ein edit-server cuda-mode counsel-etags company-jedi cmake-font-lock clang-format bind-key autopair auto-package-update auctex 0blayout)))
 '(plantuml-jar-path "/opt/plantuml/plantuml.jar")
 '(sql-electric-stuff nil)
 '(symon-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-inflection
;; used for switching between different cases, eg CamelCase,
;; lowerCamelCase, snake_case, and SCREAMING_SNAKE_CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package string-inflection
  :ensure t
  :defer t
  :bind (("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors  - https://github.com/magnars/multiple-cursors.el
;; Allows you to have multiple cursors on different lines so you can
;; easily edit multiple lines at once.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this)
         ("C-c e" . mc/edit-lines))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done 'time
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(use-package writegood-mode
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function writegood-mode "writegood-mode.el"))
  (add-hook 'org-mode-hook #'writegood-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vlf - handle open very large files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vlf
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ein - ipython notebooks in gui emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only launch if the executable exists.
;; (if (and my:jupyter_location
;;          my:jupyter_start_dir)
;;     (use-package ein
;;       :ensure t
;;       :commands (ein:jupyter-server-start)
;;       :defer 5
;;       :config
;;       (require 'ein)
;;       (require 'ein-loaddefs)
;;       (require 'ein-notebook)
;;       (require 'ein-subpackages)
;;       ;; when editing the emacs.el file, we do not want to start a new
;;       ;; Jupyter server each time we save, so we only start a new Jupyter
;;       ;; server if there currently isn't one running.
;;       (defvar my-found-ein-server nil)
;;       (dolist (my-current-process (process-list))
;;         (when (string-match "EIN: Jupyter*" (process-name my-current-process))
;;           (setq my-found-ein-server t))
;;         )
;;       (when (not my-found-ein-server)
;;         (ein:jupyter-server-start my:jupyter_location my:jupyter_start_dir))
;;       )
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Silence compiler warnings
(defvar sql-product)
(defvar sql-prompt-regexp)
(defvar sql-prompt-cont-regexp)

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (set-fill-column 1000)))

;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (electric-pair-local-mode -1)))

(add-hook 'sql-interactive-mode-hook 'my-sql-interactive-mode-hook)
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
            'my-sql-comint-preoutput-filter :append :local))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load hungry Delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set hungry delete:
(use-package hungry-delete
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-hungry-delete-mode "hungry-delete.el"))
  :config
  (global-hungry-delete-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlighting in CUDA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load CUDA mode so we get syntax highlighting in .cu files
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell Mode for Spelling Corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function flyspell-goto-next-error "flyspell.el")
    (declare-function flyspell-mode "flyspell.el")
    ;; (declare-function flyspell-prog-mode "flyspell.el")
    )
  (setq flyspell-issue-welcome-flag nil)
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))

  (global-set-key (kbd "<f8>") 'flyspell-buffer)

  (add-hook 'text-mode-hook #'flyspell-mode)
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  )
(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :requires dash
  :after (ivy)
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :config
  (add-hook 'magit-mode-hook (lambda () (setq whitespace-mode -1)))
  (setq magit-completing-read-function 'ivy-completing-read)
  )
(use-package magit-gerrit
  :ensure t
  :after magit
  )

(global-set-key (kbd "C-x g") 'magit-status)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GitGutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-gutter
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-git-gutter-mode "git-gutter.el"))
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; Auto update every 5 seconds
  (custom-set-variables
   '(git-gutter:update-interval 5))

  ;; Set the foreground color of modified lines to something obvious
  (set-face-foreground 'git-gutter:modified "purple")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (add-to-list 'company-backends 'company-cmake)))
  :config
  (use-package cmake-font-lock
    :ensure t
    :defer t
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                  1 font-lock-warning-face t)))
                          ))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windmove - enables shift + arrow keys to move between open windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode (".json" ".imp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup Dockerfile mode
;; 1. Download file from GitHub
;; 2. Load mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/dockerfile-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/spotify/dockerfile-mode/master/dockerfile-mode.el"
     "~/.emacs.d/plugins/dockerfile-mode.el"))
(use-package dockerfile-mode
  :mode ("Dockerfile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :defer 5
  :config
  (yas-global-mode t)
  (yas-reload-all))
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))
;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal configuration.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and (require 'multi-term nil t) (require 'multi-term-ext nil t))
  (global-set-key (kbd "<f5>") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-program "/bin/bash"))

(when (require 'term nil t)
  (setq term-bind-key-alist
	(list (cons "C-c C-c" 'term-interrupt-subjob)
	      (cons "C-z" 'term-stop-subjob)
	      (cons "C-r" 'term-send-raw)
	      (cons "C-s" 'term-send-raw)
	      (cons "M-f" 'term-send-forward-word)
              (cons "M-b" 'term-send-backward-word)
	      (cons "C-c C-j" 'term-line-mode)
              (cons "C-c C-k" 'term-char-mode)
	      (cons "M-DEL" 'term-send-backward-kill-word)
              (cons "M-d" 'term-send-forward-kill-word)
	      (cons "<C-left>" 'term-send-backward-word)
              (cons "<C-right>" 'term-send-forward-word)
	      (cons "C-y" 'term-paste))))

;; (add-hook 'term-mode-hook #'eterm-256color-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: info+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
  :mode ("\\.s\\'"))
;; (require 'asm-mode)
;; (add-hook 'asm-mode-hook (lambda ()
;;                            (setq indent-tabs-mode nil) ; use spaces to indent
;;                            (electric-indent-mode -1) ; indentation in asm-mode is annoying
;;                            (setq tab-stop-list (number-sequence 4 60 4))))

;; (define-key asm-mode-map (kbd "<ret>") 'newline-and-indent)
;; (define-key asm-mode-map (kbd "<backtab>") 'company-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nov: epub reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "SF UI Text"
                           :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assembly configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package x86-lookup
  :ensure t
  :config
  (setq x86-lookup-pdf "~/Documents/library/Intel Software Developer’s Manual (2017).pdf"))

;; (use-package nasm-mode
;;   :ensure t
;;   :config
;;   (add-hook 'asm-mode-hook 'nasm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SX - Stack Exchange
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sx
  :config
  (bind-keys :prefix "C-c s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syslog-mode : Package for viewing log files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'syslog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Runs latexindent on the current buffer."
  (interactive)
  (with-no-warnings
    (shell-command (concat "latexindent " (buffer-file-name) " > " (buffer-file-name) ".tmp && mv "
			   (buffer-file-name) ".tmp " (buffer-file-name)))))

(defun add-auctex-keys ()
  (local-set-key (kbd "C-c <C-i> f") 'insert-frac)
  (local-set-key (kbd "C-c <C-i> t") 'insert-text)
  (local-set-key (kbd "C-c <C-i> s") 'insert-math-subscript)
  (local-set-key (kbd "C-c <C-i> l") 'insert-left-delimiter)
  (local-set-key (kbd "C-c <C-i> r") 'insert-right-delimiter)
  (local-set-key (kbd "C-c C-f") 'indent-buffer)
  )

(add-hook 'latex-mode-hook '(lambda ()
			      (local-set-key (kbd "C-c C-f") 'latex-indent)))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; When we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :config
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-master nil
                TeX-source-correlate-start-server t
		LaTeX-indent-level 8
		)
  ;;  (cond
  ;;   ((string-equal system-type "windows-nt") ; Microsoft Windows
  ;;    (progn
  ;;      (message "Windows does not have a PDF viewer set for auctex")))
  ;;   ((string-equal system-type "darwin") ; Mac OS X
  ;;    (setq-default
  ;;     TeX-view-program-list
  ;;     '(("Skim"
  ;;        "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
  ;;       )
  ;;     TeX-view-program-selection '((output-pdf "Skim"))))
  ;;   ((string-equal system-type "gnu/linux") ; linux
  ;;    (setq-default TeX-view-program-list
  ;;                  '(("Evince" "evince --page-index=%(outpage) %o"))
  ;;                  TeX-view-program-selection '((output-pdf "Evince")))))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'add-auctex-keys)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq-default reftex-plug-into-AUCTeX t)
  )
;; Disable default syntax highlighting in certain LaTeX environments.
;; This prevents certain special characters from causing issues in those environments.
;; For instance, $ and _
;; See: https://tex.stackexchange.com/questions/111289/how-to-make-auctex-ignore-syntax-highlighting-within-environment
(setq LaTeX-verbatim-environments-local '("Verbatim" "lstlisting" "minted" "lstinline" "mintinline"))

;; (TeX-add-style-hook
;;  "latex"
;;  (lambda ()
;;    (LaTeX-add-environments
;;     '("minted" LaTeX-env-document)
;;     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use latexmk with auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mpdel : client for Mopidy music
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lib/mpdel")
(require 'mpdel)
(mpdel-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The deeper blue theme is loaded but the resulting text
;; appears black in Aquamacs. This can be fixed by setting
;; the font color under Menu Bar->Options->Appearance->Font For...
;; and then setting "Adopt Face and Frame Parameter as Frame Default"
(use-package sourcerer-theme
  :ensure t
  :config
  (load-theme 'sourcerer t))

(set-face-background 'hl-line "gray16")
;; (set-face-background 'hl-line "#372E2D")
;; The minibuffer default colors with my theme are impossible to read, so change
;; them to something better using ivy-minibuffer-match-face.
(set-cursor-color "#c2c2b0")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#222222" :foreground "#c2c2b0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 102 :width normal :foundry "PfEd" :family "SF Mono"))))
 '(auto-dim-other-buffers-face ((t (:background "#111"))))
 '(company-preview ((t (:background "#073642" :foreground "#2aa198"))))
 '(company-preview-common ((t (:foreground "#93a1a1" :underline t))))
 '(company-scrollbar-bg ((t (:background "#073642" :foreground "#2aa198"))))
 '(company-scrollbar-fg ((t (:foreground "#002b36" :background "#839496"))))
 '(company-template-field ((t (:background "#7B6000" :foreground "#073642"))))
 '(company-tooltip ((t (:background "black" :foreground "DeepSkyBlue1"))))
 '(company-tooltip-annotation ((t (:foreground "#93a1a1" :background "#073642"))))
 '(company-tooltip-common ((t (:foreground "#93a1a1" :underline t))))
 '(company-tooltip-common-selection ((t (:foreground "#93a1a1" :underline t))))
 '(company-tooltip-mouse ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
 '(company-tooltip-selection ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
 '(cursor ((t (:background "dim gray"))))
 '(header-line ((t (:background "#003366"))))
 '(term-color-red ((t (:background "#aa4450" :foreground "#aa4450"))))
 '(which-func ((t (:foreground "#8fb28f")))))

;; I don't care to see the splash screen
(setq inhibit-splash-screen t)

;; Hide the scroll bar
(scroll-bar-mode -1)
;; Ensure that scrollbar is also disabled for new frames.
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)
;; Set font size
(defvar my-font-size 80)
;; (defvar my-font-size 80)
;; Make mode bar small
(set-face-attribute 'mode-line nil  :height my-font-size)
;; Set the header bar font
(set-face-attribute 'header-line nil  :height my-font-size)
;; Set default window size and position
(setq default-frame-alist
      '((top . 0) (left . 0) ;; position
        (width . 110) (height . 90) ;; size
        ))

;; Enable line numbers on the LHS
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'pdf-outline-buffer-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'term-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'Info-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'Man-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gud-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gdb-locals-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gdb-registers-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gdb-memory-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gdb-frames-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gdb-breakpoints-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gdb-inferior-io-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'compilation-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'comint-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'special-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'message-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'Custom-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gnus-group-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'gnus-summary-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'image-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'Custom-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'sql-interactive-mode-hook (lambda () (display-line-numbers-mode -1)))
  )

;; Set the font to size 9 (90/10).
(set-face-attribute 'default nil :height my-font-size)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable which function mode and set the header line to display both the
;; path and the function we're in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (which-function-mode t)

;; (defmacro with-face
;;     (str &rest properties)
;;   `(propertize ,str 'face (list ,@properties)))

;; (defun sl/make-header ()
;;   "."
;;   (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
;;          (sl/header (file-name-directory sl/full-header))
;;          (sl/drop-str "[...]")
;;          )
;;     (if (> (length sl/full-header)
;;            (window-body-width))
;;         (if (> (length sl/header)
;;                (window-body-width))
;;             (progn
;;               (concat (with-face sl/drop-str
;;                                  :background "blue"
;;                                  :weight 'bold
;;                                  )
;;                       (with-face (substring sl/header
;;                                             (+ (- (length sl/header)
;;                                                   (window-body-width))
;;                                                (length sl/drop-str))
;;                                             (length sl/header))
;;                                  ;; :background "red"
;;                                  :weight 'bold
;;                                  )))
;;           (concat
;;            (with-face sl/header
;;                       ;; :background "red"
;;                       :foreground "red"
;;                       :weight 'bold)))
;;       (concat (if window-system ;; In the terminal the green is hard to read
;;                   (with-face sl/header
;;                              ;; :background "green"
;;                              ;; :foreground "black"
;;                              :weight 'bold
;;                              :foreground "#8fb28f"
;;                              )
;;                 (with-face sl/header
;;                            ;; :background "green"
;;                            ;; :foreground "black"
;;                            :weight 'bold
;;                            :foreground "blue"
;;                            ))
;;               (with-face (file-name-nondirectory buffer-file-name)
;;                          :weight 'bold
;;                          ;; :background "red"
;;                          )))))

;; (defun sl/display-header ()
;;   "Create the header string and display it."
;;   ;; The dark blue in the header for which-func is terrible to read.
;;   ;; However, in the terminal it's quite nice
;;   (if header-line-format
;;       nil
;;     (if window-system
;; 	(custom-set-faces
;; 	 '(which-func ((t (:foreground "#8fb28f")))))
;;       (custom-set-faces
;;        '(which-func ((t (:foreground "blue"))))))
;;     ;; Set the header line
;;     (setq header-line-format
;;           (list "-"
;; 		'(which-func-mode ("" which-func-format))
;; 		'("" ;; invocation-name
;;                   (:eval (if (buffer-file-name)
;; 			     (concat "[" (sl/make-header) "]")
;;                            "[%b]")))
;; 		)
;;           )
;;     )
;;   )
;; ;; Call the header line update
;; (add-hook 'buffer-list-update-hook
;; 	  'sl/display-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline theme where the modes are on the right side.
(use-package powerline
  :ensure t
  :config
  (defun powerline-pdf-theme ()
    "Setup a mode-line with major and minor modes on the right side."
    (interactive)
    (setq-default mode-line-format
		  '("%e"
		    (:eval
		     (let* ((active (powerline-selected-window-active))
			    (mode-line-buffer-id (if active 'mode-line-buffer-id
                                                   'mode-line-buffer-id-inactive))
			    (mode-line (if active 'mode-line 'mode-line-inactive))
			    (face0 (if active 'powerline-active0 'powerline-inactive0))
			    (face1 (if active 'powerline-active1 'powerline-inactive1))
			    (face2 (if active 'powerline-active2 'powerline-inactive2))
			    (separator-left (intern (format "powerline-%s-%s"
							    (powerline-current-separator)
							    (car powerline-default-separator-dir))))
			    (separator-right (intern (format "powerline-%s-%s"
							     (powerline-current-separator)
							     (cdr powerline-default-separator-dir))))
			    (lhs (list (powerline-raw "%*" face0 'l)
				       (powerline-buffer-size face0 'l)
				       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
				       (powerline-raw " ")
				       (funcall separator-left face0 face1)
				       (powerline-narrow face1 'l)
				       (powerline-vc face1)))
			    (center (list (powerline-raw global-mode-string face1 'r)
					  (powerline-raw '(" P" (:eval (number-to-string
                                                                        (pdf-view-current-page))))
                                                         face1 'r)
					  (powerline-raw "/" face1)
					  (powerline-raw '((:eval (number-to-string
                                                                   (pdf-cache-number-of-pages))))
                                                         face1 'r)
					  (funcall separator-right face1 face0)
					  (powerline-raw " ")
					  (powerline-raw "%6p" face0 'r)
					  (powerline-hud face2 face1)
					  (powerline-raw evil-mode-line-tag face1 'r)
					  ))
			    (rhs (list (powerline-raw " " face1)
				       (funcall separator-left face1 face2)
				       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
					 (powerline-raw erc-modified-channels-object face2 'l))
				       (powerline-major-mode face2 'l)
				       (powerline-process face2)
				       (powerline-raw " :" face2)
				       (powerline-minor-modes face2 'l)
				       (powerline-raw " " face2)
				       (funcall separator-right face2 face1)
				       ))
			    )
		       (concat (powerline-render lhs)
			       (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			       (powerline-render center)
			       (powerline-fill face1 (powerline-width rhs))
			       (powerline-render rhs))))
		    ))
    )
  (defun powerline-general-theme ()
    (interactive)
    (setq-default mode-line-format
		  '("%e"
		    (:eval
		     (let* ((active (powerline-selected-window-active))
			    (mode-line-buffer-id (if active 'mode-line-buffer-id
                                                   'mode-line-buffer-id-inactive))
			    (mode-line (if active 'mode-line 'mode-line-inactive))
			    (face0 (if active 'powerline-active0 'powerline-inactive0))
			    (face1 (if active 'powerline-active1 'powerline-inactive1))
			    (face2 (if active 'powerline-active2 'powerline-inactive2))
			    (separator-left (intern (format "powerline-%s-%s"
							    (powerline-current-separator)
							    (car powerline-default-separator-dir))))
			    (separator-right (intern (format "powerline-%s-%s"
							     (powerline-current-separator)
							     (cdr powerline-default-separator-dir))))
			    (lhs (list (powerline-raw "%*" face0 'l)
				       (powerline-buffer-size face0 'l)
				       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
				       (powerline-raw " ")
				       (funcall separator-left face0 face1)
				       (powerline-narrow face1 'l)
				       (powerline-vc face1)))
			    (center (list (powerline-raw global-mode-string face1 'r)
					  (powerline-raw "%4l" face1 'r)
					  (powerline-raw ":" face1)
					  (powerline-raw "%3c" face1 'r)
					  (funcall separator-right face1 face0)
					  (powerline-raw " ")
					  (powerline-raw "%6p" face0 'r)
					  (powerline-hud face2 face1)
					  (powerline-raw evil-mode-line-tag face1 'r)
					  ))
			    (rhs (list (powerline-raw " " face1)
				       (funcall separator-left face1 face2)
				       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
					 (powerline-raw erc-modified-channels-object face2 'l))
				       (powerline-major-mode face2 'l)
				       (powerline-process face2)
				       (powerline-raw " :" face2)
				       (powerline-minor-modes face2 'l)
				       (powerline-raw " " face2)
				       (funcall separator-right face2 face1)
				       ))
			    )
		       (concat (powerline-render lhs)
			       (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			       (powerline-render center)
			       (powerline-fill face1 (powerline-width rhs))
			       (powerline-render rhs))))
		    ))
    )
  ;; (powerline-general-theme)
  )

(provide 'init)
;;; init.el ends here
