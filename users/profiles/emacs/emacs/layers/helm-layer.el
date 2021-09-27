;;; helm-layer.el --- Helm Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def helm
  :presetup
  (:layer straight
   (straight-use-package 'helm)
   (straight-use-package 'helm-xref)
   (straight-use-package 'helm-org)
   (straight-use-package 'helm-ls-git)
   (straight-use-package 'helm-bibtex))

  :setup
  (use-package helm
    :demand t
    :hook ((helm-minibuffer-setup-hook . (lambda ()
                                           (setq-local fill-column nil)))
           ;; (helm-find-files-after-init-hook . (lambda ()
           ;;                                      ))
           )
    :config
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    (setq helm-org-format-outline-path t)
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-locate-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-lisp-fuzzy-completion t)
    (use-package helm-config)
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
          helm-autoresize-min-height 30)
    (helm-autoresize-mode 1)
    (helm-mode 1)
    ;; maximum buffer string length before truncation
    (setq helm-buffer-max-length 60)
    (setq-default helm-follow-mode-persistent t)
    ;; configure files cache
    (setq helm-ff-refresh-cache-delay 0.3)
    (setq helm-ff-cache-mode-post-delay 0.3)

    (use-package helm-regexp
      ;; :config
      ;; (setq helm-source-occur
      ;;       (helm-make-source "Occur" 'helm-source-multi-occur
      ;;         :follow 1))
      )

    ;; use ripgrep instead of ag
    (setq helm-grep-ag-command (concat "rg"
                                       " --color=never"
                                       " --smart-case"
                                       " --no-heading"
                                       " --line-number %s %s %s")))

  (use-package helm-man)

  (use-package helm-xref)

  (use-package helm-bibtex
    :config
    (setq bibtex-completion-bibliography "~/doc/notes/wiki/library.bib")
    (setq bibtex-completion-library-path "~/doc/library")
    (setq bibtex-completion-pdf-field "file")
    (setq bibtex-completion-notes-path "~/doc/notes/wiki/refs"))

  :postsetup
  (:layer modal
   (general-def mh/prefix-map
     "SPC" 'helm-M-x)
   (general-def mh/prefix-search-map
     "s" 'helm-occur)
   (general-define-key
    :keymaps 'mh/prefix-file-map
    "f" 'helm-find-files
    "a" 'helm-locate)
   (general-def mh/prefix-buffer-map
     "b" 'helm-buffers-list)
   (general-def helm-map
     "C-j" 'helm-next-line
     "C-k" 'helm-previous-line
     "C-h" 'helm-find-files-up-one-level
     "C-l" 'helm-execute-persistent-action)
   (general-def helm-read-file-map
     "C-l" 'helm-execute-persistent-action)

   ;; keybindings for candidates when invoking `helm-find-files'.
   (general-define-key
    :keymaps 'helm-find-files-map
    ;; navigate into directory at point
    "C-l" 'helm-execute-persistent-action
    "C-e" 'helm-ff-run-eshell-command-on-file
    "C-d" 'helm-ff-run-delete-file
    "C-s" 'helm-ff-run-grep
    ;; open file in adjacent window
    "C-o" 'helm-ff-run-switch-other-window
    "C-c" 'helm-ff-run-copy-file
    "C-r" 'helm-ff-run-rename-file
    "C-y" 'helm-ff-run-symlink-file
    "C-t" 'helm-ff-run-ediff-file
    "C-p" 'helm-ff-run-browse-project
    ;; display file properties
    "C-n" 'helm-ff-properties-persistent
    "M-SPC" 'mh/command-prefix)

   ;; keybindings for candidates in `helm-buffers-list'
   (general-def helm-buffer-map
     "C-d" 'helm-buffer-run-kill-persistent))

  (:layer org
   (use-package helm-org)
   (setq helm-org-headings-max-depth 100))

  (:layer vcs
   (use-package helm-ls-git
     :config
     (setq helm-locate-project-list '("~/src"))
     (setq helm-ls-git-status-command 'magit-status-setup-buffer)
     ;; search the full project, rather than just under the current directory
     (setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --full-name -e %p")))

  (:layer (vcs modal)
   (general-def helm-ls-git-map
     "M-SPC" 'mh/command-prefix
     ;; gets git status
     "C-v" (lambda ()
             (interactive)
             ;; this isn't ideal and there should be a better way to
             ;; do this, but it works
             (helm-select-nth-action 1)))

   (general-def mh/prefix-search-map
     "g" (lambda ()
           (interactive)
           (if (helm-ls-git-root-dir)
               (command-execute 'helm-grep-do-git-grep)
             (command-execute 'helm-do-grep-ag)))
     "p" 'helm-browse-project
     "P" 'helm-projects-history)))

;;; helm-layer.el ends here
