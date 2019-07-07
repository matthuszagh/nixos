;;; files-layer.el -*- lexical-binding: t; -*-

;;; Code:

(layer-def files
  :setup
  (setq wiki-file "/home/matt/doc/notes/wiki.org")
  (setq config-file "/home/matt/dotfiles/README.org")

  :postsetup
  (:layer keybinding-management
          (general-def mh/prefix-file-map
            "w" (lambda ()
                  (interactive)
                  (find-file wiki-file))
            "c" (lambda ()
                  (interactive)
                  (find-file config-file)))))

;;; files-layer.el ends here
