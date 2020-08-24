;;; files-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def files
  :setup
  (setq wiki-file "/home/matt/doc/notes/wiki.org")
  (setq system-file "/home/matt/src/dotfiles/configuration.nix")
  (setq config-file "/home/matt/src/dotfiles/config/emacs/init.el")
  (setq productivity-file "/home/matt/doc/notes/projects/productivity.org")
  (setq todo-file "/home/matt/doc/notes/todo.org")
  (setq work-file "/home/matt/doc/notes/projects/work.org")

  :postsetup
  (:layer keybinding-management
   (general-def mh/prefix-file-map
     "i" (lambda ()
           (interactive)
           (find-file wiki-file))
     "c" (lambda ()
           (interactive)
           (find-file config-file))
     "n" (lambda ()
           (interactive)
           (find-file system-file))
     "p" (lambda ()
           (interactive)
           (find-file productivity-file))
     "t" (lambda ()
           (interactive)
           (find-file todo-file))
     "w" (lambda ()
           (interactive)
           (find-file work-file)))))

;;; files-layer.el ends here
