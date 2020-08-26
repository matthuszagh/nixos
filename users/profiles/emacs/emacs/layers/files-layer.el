;;; files-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def files
  :setup
  (let ((nixos-dir "/home/matt/src/nixos")
        (notes-dir "/home/matt/doc/notes"))
    (setq wiki-file (concat notes-dir "/wiki/index.org"))
    (setq system-file (concat nixos-dir "/flake.nix"))
    (setq config-file (concat nixos-dir "/users/profiles/emacs/emacs/init.el"))
    (setq productivity-file (concat notes-dir "/projects/productivity.org"))
    (setq todo-file (concat notes-dir "/todo.org"))
    (setq work-file (concat notes-dir "/projects/work.org")))

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
