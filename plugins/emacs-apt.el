;; Copyright (C) 2012 Groza Cristian
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(defconst cache "cache"
  "For use inside functions to indicate apt module.")

(defconst get "get"
  "For use inside functions to indicate apt module.")

(defvar apt-hi-color
  "*Indicates color to be used for hightlighting.")

(defun apt-set-hi-color (hi-color)
  "apt-set-hi-color sets apt-hicolor to the value of hi-color."
  (set 'apt-hi-color hi-color))

(defun clear-buffer ()
  "Sets buffer-read-only to nil and deletes region point-min point-max"
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max)))

;; The apt-[task] functions take the necessary input and passes it to
;; apt-cmd-sync together with the respective module and task (install,
;; download etc).

(defun apt-search (names)
  "Invokes apt-cache search {names} and outputs the result."
  (interactive "sapt-cache search ")
  (apt-cmd-sync cache "search" names))

(defun apt-download (names dl-dir)
  "Changes current buffer directory to dl-dir and
   invokes apt-get download {names} and outputs the result."
  (interactive "sapt-get download \nDDownload location: ")
  (apt-cmd-async get "download" names dl-dir))

(defun apt-changelog (names)
  "Invokes apt-get changelog {names} and outputs the result."
  (interactive "sapt-get changelog ")
  (apt-cmd-sync get "changelog" names))

(defun apt-source (names dl-dir)
  "Changes current buffer directory to dl-dir and
   invokes apt-get source {names} and outputs the result."
  (interactive "sapt-get source \nDDownload location: ")
  (apt-cmd-async get "source" names dl-dir))

(defun apt-showpkg (names)
  "Invokes apt-cache showpkg {names} and outputs the result."
  (interactive "sapt-cache showpkg ")
  (apt-cmd-sync cache "showpkg" names))

(defun apt-stats (i)
  "Invokes apt-cache stats and outputs the result."
  (interactive "i")
  (apt-cmd-sync cache "stats"))

(defun apt-showsrc (i)
  "Invokes apt-cache showsrc and outputs the result."
  (interactive "i")
  (apt-cmd-sync cache "showsrc"))

(defun apt-dump (i)
  "Invokes apt-cache dump and outputs the result."
  (interactive "i")
  (apt-cmd-sync cache "dump"))

(defun apt-dumpavail (i)
  "Invokes apt-cache dumpavail and outputs the result."
  (interactive "i")
  (apt-cmd-sync cache "dumpavail"))

(defun apt-depends (names)
  "Invokes apt-cache depends {names} and outputs the result."
  (interactive "sapt-cache depends ")
  (apt-cmd-sync cache "depends" names))

(defun apt-rdepends (names)
  "Invokes apt-cache rdepends {names} and outputs the result."
  (interactive "sapt-cache rdepends ")
  (apt-cmd-sync cache "rdepends" names))

(defun apt-pkgnames (prefix)
  "Invokes apt-cache pkgnames {prefix} and outputs the result."
  (interactive "sapt-cache pkgnames ")
  (apt-cmd-sync cache "pkgnames" prefix))

(defun apt-dotty (names)
  "Invokes apt-cache dotty {names} and outputs the result."
  (interactive "sapt-cache dotty ")
  (apt-cmd-sync cache "dotty" names))

(defun apt-xvcg (names)
  "Invokes apt-cache xvcg {names} and outputs the result."
  (interactive "sapt-cache xvcg ")
  (apt-cmd-sync cache "xvcg" names))

(defun apt-policy (names)
  "Invokes apt-cache policy {names} and outputs the result."
  (interactive "sapt-cache policy ")
  (apt-cmd-sync cache "policy" names))

(defun apt-madison (names)
  "Invokes apt-cache madison {names} and outputs the result."
  (interactive "sapt-cache madison ")
  (apt-cmd-sync cache "madison" names))

(defun apt-install (names)
  "Invokes apt-get install {names}."
  (interactive "sapt-get install ")
  (async-shell-command (format  "sudo apt-get install %s" names)))

(defun apt-remove (names)
  "Invokes apt-get remove {names}."
  (interactive "sapt-get remove ")
  (async-shell-command (format  "sudo apt-get remove %s" names)))

(defun apt-purge (names)
  "Invokes apt-get purge {names}."
  (interactive "sapt-get purge ")
  (async-shell-command (format  "sudo apt-get purge %s" names)))

(defun apt-autoremove ()
  (interactive)
  "Invokes apt-get autoremove {names}."
  (async-shell-command (format  "sudo apt-get autoremove")))

(defun apt-update ()
  (interactive)
  "Invokes apt-get update."
  (async-shell-command (format  "sudo apt-get update")))

(defun apt-upgrade ()
  (interactive)
  "Invokes apt-get upgrade."
  (async-shell-command (format  "sudo apt-get upgrade")))

(defun apt-cmd-general (async? module command &optional package-names working-dir)
  "async? - async for t, sync for ()
  module - cache or get
  command - apt command such as search or pkgnames
  package-names - string containing list of packages separated by spaces
  working-dir - directory in which to execute apt
  Always returns a buffer."
  (let ((buf (get-buffer-create (format "*APT-%s %s %s%s"
                                        (upcase module) (upcase command) package-names "*")))
        (package-list (split-string (or package-names "") "\s+")))
    (switch-to-buffer-other-window buf)
    (apt-mode)
    (cd (or working-dir default-directory))
    (clear-buffer)
    ;; async or sync? Handle both cases
    (if async?
        (progn (apply 'start-process
                        "apt-get"
                        buf ;; output will be directed there
                        (format "apt-%s" module) ;; construct apt command
                        command ;; apt command
                        ;; split package list and pass it as arguments
                        package-list))
      (progn (apply 'call-process
                      (format "apt-%s" module) ;; construct apt command
                      nil ;; /dev/null
                      buf ;; output will be directed there
                      nil
                      command ;; apt command
                      ;; split package list and pass it as arguments
                      package-list)))
    (setq buffer-read-only t)
    (goto-char (point-min))
    buf))

(defun apt-cmd-async (module command &optional package-names working-dir)
  "module - cache or get
  command - apt command such as search or pkgnames
  package-names - string containing list of packages separated by spaces
  This function calls apt-cache or apt-get using call-process and returns
  the output in a buffer. Emacs may freeze until the command has finished.
  Always returns a buffer."
  (save-excursion (apt-cmd-general t module command package-names working-dir)))

(defun apt-cmd-sync (module command &optional package-names working-dir
                            highlight-func)
  "module - cache or get
  command - apt command such as search or pkgnames
  package-names - string containing list of packages separated by spaces
  working-dir - directory in which to execute apt
  highlight-func - function will to be called to perform highlighting
  This function calls apt-cache or apt-get using call-process and returns
  the output in a buffer. Emacs may freeze until the command has finished.
  Always returns a buffer."
  (save-excursion
    (let ((aptbuf (apt-cmd-general nil module command package-names working-dir))
          (package-list (split-string (or package-names "") "\s+")))
      (if highlight-func
          (funcall highlight-func)
        (highlight-regexp (regexp-opt package-list) apt-hi-color))
      aptbuf)))

(provide 'emacs-apt)
