;;; gnus --- Summary

;;; Commentary:

;;; Code:
;; -*-no-byte-compile: t; -*-

;; Save email locally.
(require 'gnus)

(setq user-mail-address "huszaghmatt@gmail.com"
      user-full-name "Matt Huszagh")

(setq nnmail-split-fancy
      '(| (any "linux-kernel@vger\\.kernel\\.org" "LKML")
          (any "emacs-devel@gnu\\.org" "emacs-devel")
          (any "kicad-developers@lists\\.launchpad\\.net" "kicad-developers")
          (any "emacs-orgmode@gnu\\.org" "emacs-orgmode")
          "mail.misc"))

(setq nnmail-split-methods 'nnmail-split-fancy)

(setq gnus-select-method '(nntp "news.gmane.io"))

;; (setq gnus-select-method
;;       '(nnimap "imap.gmail.com"
;;                (nnimap-inbox "INBOX")
;;                (nnimap-split-methods "default")
;;                (nnimap-stream ssl)
;;                (nnimap-server-port 993)))

(setq gnus-secondary-select-methods '((nnml "")))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 user-email-address nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-use-gnutls t)

;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; Replace [ and ] with _ in ADAPT file names
(setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)))

(provide '.gnus)
;;; .gnus.el ends here
