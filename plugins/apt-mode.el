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

(require 'emacs-apt)

;; syntactic sugar
(defun pkg-at-point () (thing-at-point 'symbol))

;; wrapers around the normal functions that call
;; equivalents with the symbol at point
(defun apt-mode-search ()
  "Calls apt-search with symbol at point."
  (interactive)
  (apt-search (pkg-at-point)))

(defun apt-mode-download (download-dir)
  "Calls apt-download with symbol at point."
  (interactive "DDownload location: ")
  (apt-download (pkg-at-point) download-dir))

(defun apt-mode-change-log ()
  "Calls apt-change-log with symbol at point."
  (interactive)
  (apt-change-log (pkg-at-point)))

(defun apt-mode-source (download-dir)
  "Calls apt-source with symbol at point."
  (interactive "DDownload location: ")
  (apt-source (pkg-at-point) download-dir))

(defun apt-mode-showpkg ()
  "Calls apt-showpkgs with symbol at point."
  (interactive)
  (apt-showpkg (pkg-at-point)))

(defun apt-mode-depends ()
  "Calls apt-depends with symbol at point."
  (interactive)
  (apt-depends (pkg-at-point)))

(defun apt-mode-rdepends ()
  "Calls apt-rdepends with symbol at point."
  (interactive)
  (apt-rdepends (pkg-at-point)))

(defun apt-mode-pkgnames ()
  "Calls apt-pkgnames with symbol at point."
  (interactive)
  (apt-pkgnames (pkg-at-point)))

(defun apt-mode-dotty ()
  "Calls apt-dotty with symbol at point."
  (interactive)
  (apt-dotty (pkg-at-point)))

(defun apt-mode-xvcg ()
  "Calls apt-xvcg with symbol at point."
  (interactive)
  (apt-xvcg (pkg-at-point)))

(defun apt-mode-policy ()
  "Calls apt-policy with symbol at point."
  (interactive)
  (apt-policy (pkg-at-point)))

(defun apt-mode-install ()
  "Calls apt-get install with symbol at point."
  (interactive)
  (apt-install (pkg-at-point)))

(defun apt-mode-remove ()
  "Calls apt-remove with symbol at point."
  (interactive)
  (apt-remove (pkg-at-point)))

(defun apt-mode-purge ()
  "Calls apt-purge with symbol at point."
  (interactive)
  (apt-purge (pkg-at-point)))

(defun apt-mode-madison ()
  "Calls apt-madison with symbol at point."
  (interactive)
  (apt-madison (pkg-at-point)))


(defvar apt-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "_" st)
    st) "Points are part of package names in apt.")

(defvar apt-mode-map
  (let ((map (make-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "i" 'apt-mode-install)
    (define-key map "s" 'apt-mode-search)
    (define-key map "S" 'apt-mode-source)
    (define-key map "p" 'apt-mode-pkgnames)
    (define-key map "P" 'apt-mode-policy)
    (define-key map "h" 'apt-mode-showpkg)
    (define-key map "t" 'apt-mode-dotty)
    (define-key map "x" 'apt-mode-xvcg)
    (define-key map "c" 'apt-mode-change-log)
    (define-key map "d" 'apt-mode-depends)
    (define-key map "D" 'apt-mode-download)
    (define-key map "r" 'apt-mode-rdepends)
    (define-key map "m" 'apt-mode-madison)
    map
    ))

;; define apt-mode with no self inserting keybindings
(define-derived-mode apt-mode special-mode "APT")

(provide 'apt-mode)
