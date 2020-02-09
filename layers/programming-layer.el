;;; programming-layer.el --- Programming Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def programming
  :setup
  (use-package prog-mode
    :hook
    ((prog-mode . display-line-numbers-mode)
     (prog-mode . (lambda ()
                    ;; Highlighting in cmake-mode this way interferes with
                    ;; cmake-font-lock, which is something I don't yet understand.
                    (when (not (derived-mode-p 'cmake-mode))
                      (font-lock-add-keywords
                       nil
                       '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                          1 font-lock-warning-face t))))))
     ;; Disable auto-fill.
     (prog-mode . (lambda () (auto-fill-mode -1)))))

  (use-package banner-comment)

  :postsetup
  (:layer modal
   (general-define-key
    :keymaps 'mh/prefix-prog-map
    "d" 'edebug-defun)))

;;; programming-layer.el ends here
