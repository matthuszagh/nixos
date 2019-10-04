;;; init.el --- Emacs Initialization File -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

;; Use the straight package manager instead of package.el.
;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (package-initialize)

;; ;; We use a straight-maintained mirror, which fixes an issue that makes tex-sites.el unavailable to
;; ;; AUCTeX.
;; (setq straight-recipes-gnu-elpa-use-mirror t)

;; ;; Retreive straight if we don't have it.
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)
;; ;; TODO move this to :presetup when that's working and nixpkgs is setup.
;; (straight-use-package '(layers :local-repo "layers"))

;; (load "~/src/layers/layers.el")

(add-to-list 'load-path (concat user-emacs-directory "layers"))
(require 'use-package)
(use-package layers
  :init
  (if (featurep 'straight)
      (progn
        (straight-use-package 'ht)
        (straight-use-package 'dash)))
  :config
  (declare-layers '(base
                    keybinding-management
                    modal
                    multiple-cursors
                    no-littering
                    mail
                    recoll
                    ledger
                    pinentry

                    ;; appearance
                    default-theme
                    default-mode-line
                    rainbow-delimiters

                    ;; programming
                    programming
                    vcs
                    completions
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

                    ;; formatting
                    indenting
                    formatting
                    documentation
                    undoing
                    buffers
                    files
                    helm
                    help
                    org
                    markdown
                    writing
                    exwm
                    pdf
                    epub
                    internet
                    irc))
  (declare-global-depends '(base
                            ;; straight
                            keybinding-management
                            no-littering
                            default-theme
                            default-mode-line))

  (defun mh/load-all-elisp-in-dir (dir)
    (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                    (delq nil (mapcar #'car load-history)))))
      (dolist (file (directory-files-recursively dir ".+\\.elc?$"))
        (let ((library (file-name-sans-extension file)))
          (unless (member library libraries-loaded)
            (load library nil t)
            (push library libraries-loaded))))))
  (mh/load-all-elisp-in-dir (concat user-emacs-directory "layers")))

;;; init.el ends here
