;;; c-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def c
  :setup
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
          (let ((primary-match (s-match (concat "^[ \t]*" field ":[ \t]*[0-9]+") config-str)))
            (if primary-match
                (string-to-number (car (s-match "[0-9]+" (car primary-match))))
              0))
        (let ((primary-match (s-match (concat "^[ \t]*" field ":[ \t]*[A-Za-z]+") config-str)))
          (if primary-match
              (car (s-match "[A-Za-z]+$" (car primary-match)))
            ""))))
    ;; TODO this leaves many settings unset. Look at
    ;; `c-offsets-alist', particularly in C++ for finer-tuned options.
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
                                              (progn
                                                (c-set-style "linux")
                                                (add-to-list 'c-offsets-alist '(substatement . 0))))
                                             ((equal "GNU" base-style)
                                              (c-set-style "gnu"))))))))))

  (use-package cc-mode
    ;; :after (company company-c-headers helm)
    :mode (("\\.c\\'" . c-mode)
           ("\\.cc\\'" . c++-mode)
           ("\\.cpp\\'" . c++-mode)
           ("\\.tpp\\'" . c++-mode))
    :general
    (:keymaps 'c-mode-base-map
     "RET" 'newline-and-indent)
    :hook ((c-mode-common . (lambda ()
                              (add-hook 'before-save-hook 'clang-format-buffer nil t)))
           ;; (c-mode-common . (lambda ()
           ;;                    (set (make-local-variable 'company-backends)
           ;;                         (list '(company-c-headers company-rtags company-files)))))
           )
    :config
    (setq tab-width 8)
    (setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "linux"))))

  (use-package gdb-mi
    :hook (gud-mode . (lambda ()
                        (set (make-local-variable 'company-backends) '(company-capf))))
    :config
    (add-to-list 'same-window-buffer-names "*gud.*")
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

  ;; TODO get this working.
  ;; (use-package flycheck-clang-tidy
  ;;   :straight (flycheck-clang-tidy :type git :host github :repo "matthuszagh/flycheck-clang-tidy")
  ;;   :after (flycheck)
  ;;   :hook (flycheck-mode . #'flycheck-clang-tidy-setup)
  ;;   :config
  ;;   (add-to-list 'flycheck-checkers 'c/c++-clang-tidy))

  :postsetup
  (:layer modal
   (localleader :keymaps 'c-mode-base-map
     "f" 'clang-format-buffer
     "c" (lambda (cmd)
	   (interactive
	    (list
	     (compilation-read-command compile-command)))
	   (compile cmd t))
     "k" 'kill-compilation
     "d" 'manual-entry
     "g" 'gdb))

  ;; ;; TODO get this working. Seems to be a compatibility issue with
  ;; ;; ccls. See
  ;; ;; https://github.com/alexmurray/flycheck-clang-analyzer/issues/14
  ;; (:layer flycheck
  ;;  (use-package flycheck-clang-analyzer
  ;;    :after flycheck
  ;;    :config
  ;;    (flycheck-clang-analyzer-setup)))

  (:layer lsp
   (add-hook 'c-mode-common-hook 'lsp)
   (add-hook 'c-mode-common-hook 'flycheck-mode)
   (setq mh-c-common-checks '("--all-scopes-completion"
                              ;; index in background
                              "--background-index"
                              ;; use clang-tidy
                              "--clang-tidy"
                              "--completion-style=detailed"
                              ;; insert missing headers
                              "--header-insertion=iwyu"
                              ;; suggest missing headers
                              "--suggest-missing-includes"
                              ;; use 8 cores
                              "-j=8"
                              ;; store PCH in memory for better performance
                              "--pch-storage=memory"))
   (setq mh-c-checks
         (append mh-c-common-checks
                 `(,(concat "--clang-tidy-checks=-*,clang-analyzer-*"
                            ",cert-*,bugprone-*,performance-*,portability-*"
                            ",readability-*,-clang-analyzer-cplusplus*"))))
   (setq mh-cpp-checks
         (append mh-c-common-checks
                 `(,(concat "--clang-tidy-checks=-*,clang-analyzer-*"
                            ",cert-*,bugprone-*,performance-*,portability-*"
                            ",readability-*,modernize-*,cppcoreguidelines-*"))))
   ;; TODO
   (setq lsp-clients-clangd-args mh-c-checks)
   ;; (add-hook 'c-mode-hook (lambda ()
   ;;                          (setq-local lsp-clients-clangd-args mh-c-checks)))
   ;; (add-hook 'c++-mode-hook (lambda ()
   ;;                            (setq-local lsp-clients-clangd-args mh-cpp-checks)))
   ))

;;; c-layer.el ends here
