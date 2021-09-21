;;; vcs-layer.el --- Version Control System Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def vcs
  :presetup
  (:layer straight
   (straight-use-package
    '(transient :type git :host github :repo "magit/transient"))
   (straight-use-package 'magit)
   (straight-use-package 'forge)
   (straight-use-package 'git-gutter)
   (straight-use-package 'git-timemachine)
   (straight-use-package 'projectile))
  (:layer (straight helm)
   (straight-use-package
    '(helm-projectile :type git :host github :repo "matthuszagh/helm-projectile")))

  :setup
  (use-package transient
    :config
    ;; Only display magit popup at the bottom of the current window rather than the entire frame.
    (setq transient-display-buffer-action '(display-buffer-below-selected)))

  (use-package magit
    :after (transient)
    :demand t
    ;; :after (async ghub dash git-commit with-editor transient)
    :hook ((magit-mode . (lambda ()
                           (setq whitespace-mode -1)))
           (git-commit-setup . (lambda ()
                                 (set-fill-column 70)))
           (git-commit-setup . (lambda ()
                                 (localleader
                                   :keymaps 'local
                                   "d" (lambda ()
                                         (call-interactively 'mh/insert-current-date))))))
    :commands (magit-checkout)
    :config
    (setq magit-repository-directories '(("~/src" . 10)))
    ;; (add-to-list 'display-buffer-alist
    ;;              '("magit.*"
    ;;                (magit-display-buffer)))
    (setq magit-restore-window-configuration t))

  (use-package forge
    :after magit
    :hook ((forge-post-mode . (lambda ()
                                (setq-local fill-column nil)))))

  ;; (use-package github-notifier
  ;;   :config
  ;;   (github-notifier-mode)
  ;;   (setq github-notifier-token 0))

  (use-package git-gutter
    :functions global-git-gutter-mode
    :config
    (global-git-gutter-mode t)
    (set-face-foreground 'git-gutter:modified "purple"))

  (use-package git-timemachine
    :hook (git-timemachine-mode . evil-normalize-keymaps)
    :config
    ;; Make git-timemachine work with evil.
    (evil-make-overriding-map git-timemachine-mode-map 'normal))

  :postsetup
  (:layer modal
   (general-def mh/prefix-prog-map
     "s" 'magit-status
     "S" 'magit-list-repositories
     "t" 'git-timemachine
     "e" (lambda ()
           (helm-browse-project-find-files "~/src/dotfiles/emacs"))))

  (:layer tex
   (add-hook 'LaTeX-mode 'git-gutter-mode))

  :func
  (defun mh/magit-fetch-all-repositories ()
    "Run `magit-fetch-all' in all repositories returned by `magit-list-repos`."
    (interactive)
    (dolist (repo (magit-list-repos))
      (message "Fetching in %s..." repo)
      (let ((default-directory repo))
        (magit-fetch-all (magit-fetch-arguments)))
      (message "Fetching in %s...done" repo))))

;;; vcs-layer.el ends here
