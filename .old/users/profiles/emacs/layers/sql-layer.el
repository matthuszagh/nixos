;;; sql-layer.el --- SQL Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def sql
  :presetup
  (:layer straight
   (straight-use-package 'edbi))

  :setup
  (use-package sql
    ;; Prevent auto newlines after a certain number of characters.
    :hook ((sql-interactive-mode . (lambda ()
                                     (set-fill-column 1000)))
           (sql-interactive-mode . my-sql-interactive-mode-hook))
    :config
    (add-to-list 'same-window-buffer-names "*SQL*")
    (defvar sql-product)
    (defvar sql-prompt-regexp)
    (defvar sql-prompt-cont-regexp)
    (defun my-sql-comint-preoutput-filter (output)
      "Filter prompts out of SQL query output.
Runs after `sql-interactive-remove-continuation-prompt' in
`comint-preoutput-filter-functions'."
      ;; If the entire output is simply the main prompt, return that.
      ;; (i.e. When simply typing RET at the sqli prompt.)
      (if (string-match (concat "\\`\\(" sql-prompt-regexp "\\)\\'") output)
          output
        ;; Otherwise filter all leading prompts from the output.
        ;; Store the buffer-local prompt patterns before changing buffers.
        (let ((main-prompt sql-prompt-regexp)
              (any-prompt comint-prompt-regexp) ;; see `sql-interactive-mode'
              (prefix-newline nil))
          (with-temp-buffer
            (insert output)
            (goto-char (point-min))
            (when (looking-at main-prompt)
              (setq prefix-newline t))
            (while (looking-at any-prompt)
              (replace-match ""))
            ;; Prepend a newline to the output, if necessary.
            (when prefix-newline
              (goto-char (point-min))
              (unless (looking-at "\n")
                (insert "\n")))
            ;; Return the filtered output.
            (buffer-substring-no-properties (point-min) (point-max))))))

    (defadvice sql-send-string (before my-prefix-newline-to-sql-string)
      "Force all `sql-send-*' commands to include an initial newline.

This is a trivial solution to single-line queries tripping up my
custom output filter.  (See `my-sql-comint-preoutput-filter'.)"
      (ad-set-arg 0 (concat "\n" (ad-get-arg 0))))
    (ad-activate 'sql-send-string)
    (defun my-sql-interactive-mode-hook ()
      "Custom interactive SQL mode behaviours. See `sql-interactive-mode-hook'."
      (when (eq sql-product 'postgres)
        ;; Allow symbol chars in database names in prompt.
        ;; Default postgres pattern was: "^\\w*=[#>] " (see `sql-product-alist').
        (setq sql-prompt-regexp "^\\(?:\\sw\\|\\s_\\)*=[#>] ")
        ;; Ditto for continuation prompt: "^\\w*[-(][#>] "
        (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(][#>] "))

      ;; Deal with inline prompts in query output.
      ;; Runs after `sql-interactive-remove-continuation-prompt'.
      (add-hook 'comint-preoutput-filter-functions
                'my-sql-comint-preoutput-filter :append :local))
    (defadvice sql-send-string (before my-prefix-newline-to-sql-string)
      "Force all `sql-send-*' commands to include an initial newline.

This is a trivial solution to single-line queries tripping up my
custom output filter.  (See `my-sql-comint-preoutput-filter'.)"
      (ad-set-arg 0 (concat "\n" (ad-get-arg 0))))
    (ad-activate 'sql-send-string))

  (use-package edbi))

;;; sql-layer.el ends here
