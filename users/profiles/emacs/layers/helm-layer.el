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
   (straight-use-package 'helm-rg)
   (straight-use-package 'helm-projectile))

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
    (setq helm-buffer-max-length 30)
    (setq-default helm-follow-mode-persistent t)

    (use-package helm-regexp
      ;; :config
      ;; (setq helm-source-occur
      ;;       (helm-make-source "Occur" 'helm-source-multi-occur
      ;;         :follow 1))
      )

    (use-package helm-rg))

  (use-package helm-man)

  (use-package helm-xref)

  (use-package helm-projectile
     :config
     (helm-projectile-on)
     (setq helm-projectile-truncate-lines t)
     (setq projectile-completion-system 'helm))

  :postsetup
  (:layer modal
   (general-def mh/prefix-map
     "SPC" 'helm-M-x)
   (general-def mh/prefix-search-map
     "s" 'helm-occur)
   (general-define-key
    :keymaps 'mh/prefix-file-map
    "f" 'helm-find-files
    "l" 'helm-locate)
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
    "C-n" 'helm-ff-properties-persistent)

   ;; keybindings for helm-projectile
   (general-def mh/prefix-search-map
     "p" 'helm-projectile
     "P" 'helm-projectile-rg)

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
     (setq helm-ls-git-status-command 'magit-status-internal)))

  (:layer (vcs modal)
   (general-def helm-ls-git-map
     ;; TODO what's the right command for this?
     "C-s" 'helm-ls-git-status
     "C-s" (lambda ()
             (call-interactively 'helm-ff-run-eshell-command-on-file "magit")))))

;;; helm-layer.el ends here
