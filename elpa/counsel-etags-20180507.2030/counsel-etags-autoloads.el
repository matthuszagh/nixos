;;; counsel-etags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "counsel-etags" "counsel-etags.el" (23298 5502
;;;;;;  883889 509000))
;;; Generated autoloads from counsel-etags.el

(autoload 'counsel-etags-get-hostname "counsel-etags" "\
Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified

\(fn)" nil nil)

(autoload 'counsel-etags-directory-p "counsel-etags" "\
Does directory of current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-filename-p "counsel-etags" "\
Does current file match REGEX?

\(fn REGEX)" nil nil)

(autoload 'counsel-etags-update-tags-force "counsel-etags" "\
Update tags file now.

\(fn)" t nil)

(autoload 'counsel-etags-scan-code "counsel-etags" "\
Use Ctags to scan code at DIR.

\(fn &optional DIR)" t nil)

(autoload 'counsel-etags-list-tag "counsel-etags" "\
List all tags.

\(fn)" t nil)

(autoload 'counsel-etags-find-tag "counsel-etags" "\
Find tag by two step matching.

First, user need input regex to fuzzy match tag.
Any tag whose sub-string matches regex will be listed.

Second, user could filter tags.

\(fn)" t nil)

(autoload 'counsel-etags-find-tag-at-point "counsel-etags" "\
Find tag using tagname at point.

\(fn)" t nil)

(autoload 'counsel-etags-recent-tag "counsel-etags" "\
Find tag using tagname from `counsel-etags-tag-history'.

\(fn)" t nil)

(autoload 'counsel-etags-virtual-update-tags "counsel-etags" "\
Scan the code and create tags file again.  Please note it's only interface
used by other hooks or commands.  The tags updating might now happen.

\(fn)" t nil)

(autoload 'counsel-etags-grep "counsel-etags" "\
Grep at project root directory or current directory.
Try to find best grep program (ripgrep, grep...) automatically.
Extended regex like (pattern1|pattern2) is used.
If DEFAULT-KEYWORD is not nil, it's used as grep keyword.
If HINT is not nil, it's used as grep hint.

\(fn &optional DEFAULT-KEYWORD HINT)" t nil)

(autoload 'counsel-etags-grep-symbol-at-point "counsel-etags" "\
Similar to `counsel-etags-grep' but grep symbol at point.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; counsel-etags-autoloads.el ends here
