;;; vcs-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def vcs
  :presetup
  (:layer straight
          (straight-use-package
           '(transient :type git :host github :repo "magit/transient"))
          (straight-use-package 'magit)
          (straight-use-package 'git-gutter)
          (straight-use-package 'git-timemachine)
          (straight-use-package 'projectile)
          ;; TODO this should also depend on helm layer
          (straight-use-package 'helm-projectile)
          ;; TODO this should also depend on modal-interaction layer
          (straight-use-package 'evil-magit))

  :setup
  (use-package transient
    :config
    ;; Only display magit popup at the bottom of the current window rather than the entire frame.
    (setq transient-display-buffer-action '(display-buffer-below-selected)))

  (use-package magit
    :after (transient)
    ;; :after (async ghub dash git-commit with-editor transient)
    :hook ((magit-mode . (lambda ()
                           (setq whitespace-mode -1)))
           (git-commit-setup . (lambda ()
                                 (set-fill-column 50))))
    :commands (magit-checkout))

  (use-package git-gutter
    :functions global-git-gutter-mode
    :config
    ;; If you enable global minor mode
    (global-git-gutter-mode t)
    ;; Set the foreground color of modified lines to something obvious
    (set-face-foreground 'git-gutter:modified "purple"))

  (use-package git-timemachine
    :hook (git-timemachine-mode . evil-normalize-keymaps)
    :config
    ;; Make git-timemachine work with evil.
    (evil-make-overriding-map git-timemachine-mode-map 'normal))

  (use-package projectile
    :config
    (projectile-global-mode))

  :postsetup
  (:layer helm
          (use-package helm-projectile
            :config
            (helm-projectile-on)
            (setq helm-projectile-truncate-lines t)
            (setq projectile-completion-system 'helm)))

  (:layer (helm modal-interaction)
          (general-def mh/prefix-search-map
            "p" 'helm-projectile))

  (:layer modal-interaction
          (use-package evil-magit
            :after (magit transient))
          (general-def mh/prefix-prog-map
            "s" 'magit-status)))

;;; vcs-layer.el ends here
