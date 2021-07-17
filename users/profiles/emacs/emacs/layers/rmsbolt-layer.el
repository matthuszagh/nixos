;;; rmsbolt-layer.el --- RMSbolt Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def rmsbolt
  :depends (programming)

  :presetup
  (:layer straight
   (straight-use-package '(rmsbolt :type git :host gitlab :repo "matthuszagh/rmsbolt")))

  :setup
  (use-package rmsbolt
    :config
    (setq rmsbolt-asm-format nil)
    (setq
     rmsbolt-languages
     `((c-mode
        . ,(make-rmsbolt-lang :compile-cmd "gcc"
                              :supports-asm t
                              :supports-disass t
                              :demangler "c++filt"
                              :compile-cmd-function #'rmsbolt--c-compile-cmd
                              :disass-hidden-funcs rmsbolt--hidden-func-c))
       (c++-mode
        . ,(make-rmsbolt-lang :compile-cmd "g++"
                              :supports-asm t
                              :supports-disass t
                              :demangler "c++filt"
                              :compile-cmd-function #'rmsbolt--c-compile-cmd
                              :disass-hidden-funcs rmsbolt--hidden-func-c))
       (d-mode
        . ,(make-rmsbolt-lang :compile-cmd "ldc2"
                              :supports-asm t
                              :supports-disass nil
                              :demangler "ddemangle"
                              :compile-cmd-function #'rmsbolt--d-compile-cmd))
       ;; In order to parse ocaml files, you need the emacs ocaml mode, tuareg
       (tuareg-mode
        . ,(make-rmsbolt-lang :compile-cmd "ocamlopt"
                              :supports-asm t
                              :supports-disass t
                              :compile-cmd-function #'rmsbolt--ocaml-compile-cmd
                              :disass-hidden-funcs rmsbolt--hidden-func-ocaml))
       (lisp-mode
        . ,(make-rmsbolt-lang :compile-cmd "sbcl"
                              :supports-asm t
                              :supports-disass nil
                              :objdumper 'cat
                              :compile-cmd-function #'rmsbolt--lisp-compile-cmd))
       (rust-mode
        . ,(make-rmsbolt-lang :compile-cmd "cargo rustc --release --"
                              :supports-asm t
                              :supports-disass nil
                              :objdumper 'objdump
                              :demangler "rustfilt"
                              :compile-cmd-function #'rmsbolt--rust-compile-cmd))
       (rustic-mode
        . ,(make-rmsbolt-lang :compile-cmd "cargo rustc --release --"
                              :supports-asm t
                              :supports-disass nil
                              :objdumper 'objdump
                              :demangler "rustfilt"
                              :compile-cmd-function #'rmsbolt--rust-compile-cmd))
       (ponylang-mode
        . ,(make-rmsbolt-lang :compile-cmd "ponyc"
                              :supports-asm t
                              :supports-disass t
                              :objdumper 'objdump
                              :compile-cmd-function #'rmsbolt--pony-compile-cmd))
       (php-mode
        . ,(make-rmsbolt-lang :compile-cmd #'rmsbolt--php-default-compile-cmd
                              :supports-asm t
                              :supports-disass nil
                              :compile-cmd-function #'rmsbolt--php-compile-cmd
                              :process-asm-custom-fn #'rmsbolt--process-php-bytecode))
       ;; ONLY SUPPORTS PYTHON 3
       (python-mode
        . ,(make-rmsbolt-lang :compile-cmd "python3"
                              :supports-asm t
                              :supports-disass nil
                              :compile-cmd-function #'rmsbolt--py-compile-cmd
                              :process-asm-custom-fn #'rmsbolt--process-python-bytecode))
       (haskell-mode
        . ,(make-rmsbolt-lang :compile-cmd "ghc"
                              :supports-asm t
                              :supports-disass nil
                              :demangler "haskell-demangler"
                              :compile-cmd-function #'rmsbolt--hs-compile-cmd))
       (java-mode
        . ,(make-rmsbolt-lang :compile-cmd "javac"
                              :supports-asm t
                              :supports-disass nil
                              :objdumper 'cat
                              :compile-cmd-function #'rmsbolt--java-compile-cmd
                              :process-asm-custom-fn #'rmsbolt--process-java-bytecode))
       (emacs-lisp-mode
        . ,(make-rmsbolt-lang :supports-asm t
                              :supports-disass nil
                              ;; Nop
                              :process-asm-custom-fn (lambda (_src-buffer lines)
                                                       lines)
                              :elisp-compile-override #'rmsbolt--elisp-compile-override))
       (zig-mode
        . ,(make-rmsbolt-lang :compile-cmd "zig"
                              :supports-asm t
                              :supports-disass t
                              :objdumper 'objdump
                              :compile-cmd-function #'rmsbolt--zig-compile-cmd
                              :disass-hidden-funcs rmsbolt--hidden-func-zig))
       (go-mode
        . ,(make-rmsbolt-lang :compile-cmd "go"
                              :supports-asm nil
                              :supports-disass t
                              :objdumper 'go-objdump
                              :compile-cmd-function #'rmsbolt--go-compile-cmd
                              :process-asm-custom-fn #'rmsbolt--process-go-asm-lines))
       (swift-mode
        . ,(make-rmsbolt-lang :compile-cmd (rmsbolt--path-to-swift-compiler)
                              :supports-asm t
                              :supports-disass nil
                              :objdumper 'objdump
                              :demangler (rmsbolt--path-to-swift-demangler)
                              :compile-cmd-function #'rmsbolt--swift-compile-cmd))))
    (defun mh/rmsbolt-compile ()
      "Compile the current rmsbolt buffer."
      (interactive)
      (save-some-buffers nil (lambda () rmsbolt-mode))
      (rmsbolt--gen-temp)
      ;; Current buffer = src-buffer at this point
      (setq-local rmsbolt-src-buffer (current-buffer))
      (cond
       ((eq major-mode 'asm-mode)
        ;; We cannot compile asm-mode files
        (message "Cannot compile assembly files. Are you sure you are not in the output buffer?"))
       ((rmsbolt-l-elisp-compile-override (rmsbolt--get-lang))
        (funcall
         (rmsbolt-l-elisp-compile-override (rmsbolt--get-lang))
         :src-buffer (current-buffer)))
       (t
        (rmsbolt--parse-options)
        (let* ((src-buffer (current-buffer))
               (lang (rmsbolt--get-lang))
               (func (rmsbolt-l-compile-cmd-function lang))
               ;; Generate command
               (cmd (funcall func :src-buffer src-buffer))
               (asm-format
                (buffer-local-value 'rmsbolt-asm-format src-buffer))
               (default-directory (or rmsbolt-default-directory
                                      rmsbolt--temp-dir)))
          (when (buffer-local-value 'rmsbolt-disassemble src-buffer)
            (pcase
                (rmsbolt-l-objdumper lang)
              ('objdump
               (setq cmd
                     (mapconcat #'identity
                                (list cmd
                                      "&&"
                                      "objdump" "-d" (rmsbolt-output-filename src-buffer)
                                      "-C" "--insn-width=16" "-l"
                                      (when (not (booleanp asm-format))
                                        (concat "-M " asm-format))
                                      ">" (rmsbolt-output-filename src-buffer t))
                                " ")))
              ('go-objdump
               (setq cmd
                     (mapconcat #'identity
                                (list cmd
                                      "&&"
                                      "go" "tool"
                                      "objdump" (rmsbolt-output-filename src-buffer)
                                      ">" (rmsbolt-output-filename src-buffer t))
                                " ")))
              ('cat
               (setq cmd
                     (mapconcat #'identity
                                (list cmd
                                      "&&" "mv"
                                      (rmsbolt-output-filename src-buffer)
                                      (rmsbolt-output-filename src-buffer t))
                                " ")))
              (_
               (error "Objdumper not recognized"))))
          ;; Convert to demangle if we need to
          (setq cmd (rmsbolt--demangle-command cmd lang src-buffer))
          (rmsbolt-with-display-buffer-no-window
           (let ((shell-file-name (or (executable-find rmsbolt--shell)
                                      shell-file-name)))
             (with-current-buffer
                 ;; TODO should this be configurable?
                 (compilation-start cmd nil (lambda (&rest _) "*rmsbolt-compilation*"))
               (add-hook 'compilation-finish-functions
                         #'rmsbolt--handle-finish-compile nil t)
               (setq-local rmsbolt-src-buffer src-buffer))))))))
    )

  :postsetup
  (:layer c
   (add-hook 'c-mode-common-hook 'rmsbolt-mode)
   (add-hook 'c-mode-common-hook
             (lambda ()
               (setq-local rmsbolt-command "clang -O0 -target riscv64"))))

  (:layer (c modal)
   (localleader :keymaps 'c-mode-base-map
     "C" 'rmsbolt-compile))

  (:layer elisp
   (add-hook 'emacs-lisp-mode-hook 'rmsbolt-mode))
  (:layer (elisp modal)
   (localleader :keymaps 'elisp-mode-map
     "C" 'rmsbolt-compile))

  (:layer python
   (add-hook 'python-mode-hook 'rmsbolt-mode))
  (:layer (python modal)
   (localleader :keymaps 'python-mode-map
     "C" 'rmsbolt-compile))

  (:layer rust
   (add-hook 'rustic-mode-hook 'rmsbolt-mode))

  (:layer (rust modal)
   (localleader :keymaps 'rust-mode-map
     "C" 'rmsbolt-compile)))

;;; rmsbolt-layer.el ends here
