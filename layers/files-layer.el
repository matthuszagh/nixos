;;; files-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def files
  :setup
  (setq wiki-file "/home/matt/doc/notes/wiki.org")
  (setq config-file "/home/matt/src/dotfiles/config/emacs/init.el")
  (setq productivity-file "/home/matt/doc/notes/projects/productivity.org")
  (setq work-file "/home/matt/doc/notes/projects/work.org")

  :postsetup
  (:layer keybinding-management
   (general-def mh/prefix-file-map
     "w" (lambda ()
           (interactive)
           (find-file wiki-file))
     "c" (lambda ()
           (interactive)
           (find-file config-file))
     "p" (lambda ()
           (interactive)
           (find-file productivity-file))
     "t" (lambda ()
           (interactive)
           (find-file work-file)))))

;;; files-layer.el ends here
