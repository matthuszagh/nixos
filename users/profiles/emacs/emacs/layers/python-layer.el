;;; python-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def python
  :depends (snippet)

  :presetup
  (:layer straight
   (straight-use-package 'cython-mode)
   (straight-use-package 'flycheck-cython)
   (straight-use-package 'blacken)
   (straight-use-package 'python-docstring)
   (with-eval-after-load 'yasnippet
     (straight-use-package 'elpy)))

  :setup
  (use-package python
    :mode (("\\.py\\'" . python-mode))
    :hook ((python-mode . (lambda ()
                            (setq-local tab-width 4)
                            (highlight-indentation-mode 0))))
    :config
    (setq-default python-indent 4)
    (setq-default python-indent-offset 4)
    (setq-default pdb-command-name "python -m ipdb")
    ;; Use ipython as the python shell interpreter.
    ;; (setq python-shell-interpreter "ipython3")
    ;; (setq python-shell-interpreter-args "console --simple-prompt")
    (setq python-shell-interpreter "python")
    (setq python-shell-interpreter-args "")
    (setq python-shell-prompt-detect-failure-warning nil)
    (add-to-list 'same-window-buffer-names "*Python*")
    ;; (setq python-shell-interpreter "jupyter"
    ;;       python-shell-interpreter-args "console --simple-prompt"
    ;;       python-shell-prompt-detect-failure-warning nil)
    ;; ;; TODO why is this disabled?
    ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
    ;;              "jupyter")

    ;; uses a nicer style for python docstrings. See documentation for
    ;; details.
    (setq python-fill-docstring-style 'django)
    )

  (use-package elpy
    :hook (python-mode . elpy-mode)
    :config
    (elpy-enable))
  ;; (setq elpy-rpc-backend "jedi")
  ;; (setq elpy-shell-display-buffer-after-send t)
  ;; (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules))
  ;; (when (require 'flycheck nil t)
  ;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;;   (setq flycheck-python-pylint-executable "pylint")
  ;;   (add-hook 'elpy-mode-hook 'flycheck-mode)))

  ;; (use-package py-autopep8
  ;;   :hook (elpy-mode . py-autopep8-enable-on-save))

  ;; (use-package yapfify
  ;;   :hook (python-mode . yapf-mode))

  (use-package blacken
    :hook (python-mode . blacken-mode)
    :config
    (setq blacken-line-length 79)
    (setq blacken-only-if-project-is-blackened nil))

  (use-package python-docstring
    :hook (python-mode . (lambda ()
                           (python-docstring-mode))))

  (use-package cython-mode)
  (use-package flycheck-cython)

  :postsetup
  (:layer debugging
   (localleader :keymaps 'python-mode-map
     "d" 'realgud:pdb
     "c" '(lambda (cmd)
            (interactive
             (list
              (compilation-read-command compile-command)))
            (compile cmd t)))))

;;; python-layer.el ends here
