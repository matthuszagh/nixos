;;; pdf-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def pdf
  :setup
  ;; TODO setup mouseless link clicking. Apparently, pdf-tools can
  ;; find links and present them as key sequences in the same way
  ;; vimium does. See pdf-links-action-perform. Currently imagemagick
  ;; is spitting out errors related to fonts. Fix this and set it up.

  ;; TODO change search keybinding.
  (use-package pdf-tools
    :demand t
    :mode "\\.pdf\\'"
    :init
    ;; Only warn when opening files larger than 1GB. This is particularly useful for PDFs which are
    ;; often larger than the default threshold.
    (setq large-file-warning-threshold nil)
    :config
    (pdf-tools-install)
    ;; fixes an issue in emacs 27 where pdf is blurry and too large otherwise.
    (setq image-scaling-factor 1)

    ;; TODO remove if this pr accepted: https://github.com/politza/pdf-tools/pull/602
    (defun mh/pdf-outline-noselect (&optional buffer)
      "Create an PDF outline of BUFFER, but don't display it."
      (save-current-buffer
        (and buffer (set-buffer buffer))
        (pdf-util-assert-pdf-buffer)
        (let* ((pdf-buffer (current-buffer))
               (pdf-file (pdf-view-buffer-file-name))
               (pdf-window (and (eq pdf-buffer (window-buffer))
                                (selected-window)))
               (bname (pdf-outline-buffer-name))
               (buffer-exists-p (get-buffer bname))
               (buffer (get-buffer-create bname)))
          (with-current-buffer buffer
            (setq-local fill-column nil)
            (unless buffer-exists-p
              (when (= 0 (save-excursion
                           (pdf-outline-insert-outline pdf-buffer)))
                (kill-buffer buffer)
                (error "PDF has no outline"))
              (pdf-outline-buffer-mode))
            (set (make-local-variable 'other-window-scroll-buffer)
                 pdf-buffer)
            (setq pdf-outline-pdf-window pdf-window
                  pdf-outline-pdf-document (or pdf-file pdf-buffer))
            (current-buffer)))))

    (advice-add 'pdf-outline-noselect :override #'mh/pdf-outline-noselect)
    ;; end TODO

    (setq-default pdf-view-display-size 'fit-page))

  :postsetup
  (:layer modal
   (general-define-key
    :states 'normal
    :keymaps 'pdf-view-mode-map
    "j" 'pdf-view-next-line-or-next-page
    "k" 'pdf-view-previous-line-or-previous-page
    "SPC" 'mh/command-prefix
    "g l" 'pdf-view-goto-page)

   (general-define-key
    :state 'normal
    :keymaps 'pdf-outline-buffer-mode-map
    "<tab>" 'outline-toggle-children)

   (localleader :keymaps 'pdf-view-mode-map
     "s" 'pdf-occur
     "g" 'pdf-view-goto-page
     "l" 'pdf-links-isearch-link)))

;;; pdf-layer.el ends here
