;;; init.el --- Emacs Initialization File -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

;; add a new profile pointing to the emacs source tree
(setq mh-emacs-directory "~/src/nixos/users/profiles/emacs/emacs")

;; Navigate straight to source directory for lockfile. This is under
;; version control and writable, unlike the read-only copy (nix store
;; symlink) under ~/.config/emacs.
(setq straight-profiles `((nil . ,(concat mh-emacs-directory "/straight/versions/default.el"))))

(straight-use-package 'use-package)

(require 'use-package)

;; Prefer `.el' files over outdated `.elc' files. Use this with `auto-compile' to automatically
;; byte-compile outdated files.
(straight-use-package 'auto-compile)
(setq load-prefer-newer t)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; TODO move this to :presetup when that's working and nixpkgs is setup.
;; (straight-use-package
;;  '(layers :type git :host github :repo "matthuszagh/layers"))
(straight-use-package 'ht)
(straight-use-package 'dash)
(straight-use-package '(layers :local-repo "~/src/layers"))

(use-package layers
  :init
  (if (featurep 'straight)
      (progn
        (straight-use-package 'ht)
        (straight-use-package 'dash)))
  :config
  (declare-layers '(base
                    keybinding-management
                    straight
                    modal
                    multiple-cursors
                    no-littering
                    mail
                    recoll
                    ledger
                    pinentry
                    image
                    time
                    calendar

                    ;; appearance
                    ;; sourcerer-theme
                    naysayer-theme
                    ;; TODO spaceline breaks emacs knowing the correct window. See `frame-selected-window' and `powerline-selected-window'.
                    ;; spaceline
                    rainbow-delimiters
                    ;; pairs
                    async

                    ;; programming
                    programming
                    vcs
                    completions
                    prescient
                    shell
                    assembly
                    nix
                    c
                    lsp
                    ;; ccls
                    cmake
                    sysadmin
                    octave
                    clisp
                    elisp
                    lisp
                    flycheck
                    refactor
                    dumb-jump
                    calc
                    verilog
                    python
                    sx
                    elfeed
                    make
                    debugging
                    sage
                    tex
                    sql
                    scad
                    hexl
                    rust
                    haskell
                    rmsbolt
                    asy
                    snippet
                    dap
                    spice
                    hydra
                    ;; TODO timeout errors
                    ;; perspective
                    json
                    yaml
                    web-dev
                    diff
                    jupyter
                    grammar

                    ;; formatting
                    indenting
                    formatting
                    documentation
                    undoing
                    buffers
                    line
                    windows
                    files
                    helm
                    help
                    org
                    org-roam
                    org-noter
                    org-ref
                    markdown
                    writing
                    exwm
                    pdf
                    epub
                    internet
                    irc
                    icon-font))
  (declare-global-depends '(base
                            straight
                            keybinding-management
                            no-littering))

  (defun mh/load-all-elisp-in-dir (dir)
    (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                    (delq nil (mapcar #'car load-history)))))
      (dolist (file (directory-files-recursively dir ".+-layer\\.elc?$"))
        (let ((library (file-name-sans-extension file)))
          (unless (member library libraries-loaded)
            (load library nil t)
            (push library libraries-loaded))))))
  (mh/load-all-elisp-in-dir (concat user-emacs-directory "layers")))

;;; init.el ends here
