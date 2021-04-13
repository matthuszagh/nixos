;;; cmake-layer.el --- CMake Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def cmake
  :presetup
  (:layer straight
   (straight-use-package 'cmake-mode)
   (straight-use-package 'cmake-font-lock))

  :setup
  (use-package cmake-mode
    :mode ("CMakeLists.txt" "\\.cmake\\'")
    :hook (cmake-mode . (lambda ()
                          (set (make-local-variable 'company-backends)
                               (list
                                (cons 'company-cmake company-backends)))))
    :config
    (use-package cmake-font-lock
      :defer t
      :commands (cmake-font-lock-activate)
      :hook (cmake-mode . (lambda ()
                            (cmake-font-lock-activate)
                            (font-lock-add-keywords
                             nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                    1 font-lock-warning-face t))))))))

;;; cmake-layer.el ends here
