;;; shell-layer.el -*-lexical-binding: t; -*-

;;; Code:

(layer-def shell
  :depends (programming)

  :setup
  (use-package shell
    :hook
    ((shell-mode . (lambda ()
                     (setq-local scroll-margin 0)
                     (setq-local scroll-conservatively 101))))
    :config
    (add-to-list 'same-window-buffer-names "*shell*"))

  (use-package eshell
    :hook
    ((eshell-mode . (lambda ()
                      (setq-local scroll-margin 0)
                      (setq-local scroll-conservatively 101)))))

  (use-package load-bash-alias
    :ensure t
    :config
    (setq load-bash-alias-bashrc-file "~/.bashrc"))

  :postsetup
  (:layer keybinding-management
          (general-def mh/prefix-map
            "c" 'async-shell-command
            "C" 'eshell))
  ;; (:layer completions
  ;;         (straight-use-package 'bash-completion)
  ;;         (use-package bash-completion
  ;;           :config
  ;;           (bash-completion-setup)))
  )

;;; shell-layer.el ends here
