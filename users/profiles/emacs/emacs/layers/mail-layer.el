;;; mail-layer.el --- Mail -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def mail
  :presetup
  (:layer straight
   (straight-use-package 'notmuch)
   (straight-use-package 'helm-notmuch))

  :setup
  (use-package notmuch
    :config
    ;; setup the mail address and use name
    (setq mail-user-agent 'message-user-agent)
    (setq user-mail-address "huszaghmatt@gmail.com"
          user-full-name "Matt Huszagh")
    ;; smtp config
    (setq smtpmail-smtp-server "smtp.gmail.com"
          message-send-mail-function 'message-smtpmail-send-it)
    (setq smtpmail-smtp-service 587)

    ;; report problems with the smtp server
    (setq smtpmail-debug-info t)
    ;; add Cc and Bcc headers to the message buffer
    (setq message-default-mail-headers "Cc: \nBcc: \n")
    ;; postponed message is put in the following draft directory
    (setq message-auto-save-directory "~/mail/draft")
    (setq message-kill-buffer-on-exit t)
    ;; change the directory to store the sent mail
    (setq message-directory "~/mail/")
    ;; display newest messages first
    (setq notmuch-search-oldest-first nil))

  :postsetup
  (:layer helm
   (use-package helm-notmuch))

  (:layer (helm modal)
   (general-def mh/prefix-search-map
     "m" 'helm-notmuch))

  (:layer modal
   (localleader :keymaps 'notmuch-show-mode-map
     "r" 'notmuch-show-reply)
   (localleader :keymaps 'notmuch-message-mode-map
     "a" 'mml-attach-file)))

;; :func
;; (defun mh/notmuch-exec-offlineimap ()
;;   "execute offlineimap"
;;   (interactive)
;;   (set-process-sentinel
;;    (start-process-shell-command "offlineimap"
;;                                 "*offlineimap*"
;;                                 "offlineimap -o")
;;    '(lambda (process event)
;;       (notmuch-refresh-all-buffers)
;;       (let ((w (get-buffer-window "*offlineimap*")))
;;         (when w
;;           (with-selected-window w (recenter (window-end)))))))
;;   (popwin:display-buffer "*offlineimap*"))

;; (add-to-list 'popwin:special-display-config
;;              '("*offlineimap*" :dedicated t :position bottom :stick t
;;                :height 0.4 :noselect t)))

;;; mail-layer.el ends here
