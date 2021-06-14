;;; pdf-layer.el --- PDF Layer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def pdf
  :presetup
  (:layer straight
   ;; use nix provided pdf-tools
   (straight-use-package '(pdf-tools :type built-in)))

  :setup
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
    ;; do not limit max page/image size
    (setq pdf-view-max-image-width 100000)

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
    "g l" 'pdf-view-goto-page
    "l" 'image-forward-hscroll
    "L" (lambda ()
          (interactive)
          (funcall-interactively 'image-forward-hscroll 10))
    "h" 'image-backward-hscroll
    "H" (lambda ()
          (interactive)
          (funcall-interactively 'image-backward-hscroll 10)))

   (general-define-key
    :state 'normal
    :keymaps 'pdf-outline-buffer-mode-map
    "<tab>" 'outline-toggle-children)

   (localleader :keymaps 'pdf-view-mode-map
     "s" 'pdf-occur
     "g" 'pdf-view-goto-page
     "l" 'pdf-links-isearch-link
     "h" 'pdf-view-fit-height-to-window
     "w" 'pdf-view-fit-width-to-window))

  :func
  (defun mh//pdf-view--rotate (&optional counterclockwise-p page-p)
    "Rotate PDF 90 degrees. Requires pdftk to work.\n
Clockwise rotation is the default; set COUNTERCLOCKWISE-P to
non-nil for the other direction.  Rotate the whole document by
default; set PAGE-P to non-nil to rotate only the current page.
\nWARNING: overwrites the original file, so be careful!"
    ;; error out when pdftk is not installed
    (if (null (executable-find "pdftk"))
        (error "Rotation requires pdftk")
      ;; only rotate in pdf-view-mode
      (when (eq major-mode 'pdf-view-mode)
        (let* ((rotate (if counterclockwise-p "left" "right"))
               (file   (format "\"%s\"" (pdf-view-buffer-file-name)))
               (page   (pdf-view-current-page))
               (pages  (cond ((not page-p)                        ; whole doc?
                              (format "1-end%s" rotate))
                             ((= page 1)                          ; first page?
                              (format "%d%s %d-end"
                                      page rotate (1+ page)))
                             ((= page (pdf-info-number-of-pages)) ; last page?
                              (format "1-%d %d%s"
                                      (1- page) page rotate))
                             (t                                   ; interior page?
                              (format "1-%d %d%s %d-end"
                                      (1- page) page rotate (1+ page))))))
          ;; empty string if it worked
          (if (string= "" (shell-command-to-string
                           (format (concat "pdftk %s cat %s "
                                           "output %s.NEW "
                                           "&& mv %s.NEW %s")
                                   file pages file file file)))
              (pdf-view-revert-buffer nil t)
            (error "Rotation error!"))))))

  (defun mh/pdf-view-rotate-clockwise (&optional arg)
    "Rotate PDF page 90 degrees clockwise.  With prefix ARG, rotate
entire document."
    (interactive "P")
    (mh//pdf-view--rotate nil (not arg)))

  (defun mh/pdf-view-rotate-counterclockwise (&optional arg)
    "Rotate PDF page 90 degrees counterclockwise.  With prefix ARG,
rotate entire document."
    (interactive "P")
    (mh//pdf-view--rotate :counterclockwise (not arg)))

  (defun mh/pdf-outline-to-k2pdfopt-input ()
    "Take an existing PDF outline and generate an equivalent input in the format expected by k2pdfopt.
The outline display uses a line for each outline entry. The page
number is displayed at the end of the line in parentheses. The
line has two spaces at the beginning for each sublevel. For example:

RF Digital Troubleshooting (63)
Removal & Replacement (67)
  RF Section Fuses (67)
  Coax Switch & BPF (68)

The k2pdfopt input places the page number at the beginning of the
line after all + symbols and uses a + for each sublevel (no + for
top level). For example:

63 RF Digital Troubleshooting
67 Removal & Replacement
+67 RF Section Fuses
+68 Coax Switch & BPF

Therefore, we should replace every consecutive 2 spaces by a '+'
and then move the page number to after the pluses.
"
    (interactive)
    (let* ((outline-buffer (buffer-name))
           (buffer-sha (buffer-hash))
           (k2pdfopt-buffer (get-buffer-create "*outline k2pdfopt*")))
      (with-current-buffer k2pdfopt-buffer
        (delete-region (point-min) (point-max)))
      (with-current-buffer outline-buffer
        (copy-to-buffer k2pdfopt-buffer (point-min) (point-max)))
      (with-current-buffer k2pdfopt-buffer
        (goto-char (point-min))
        ;; Replace all double spaces not at the beginning of the current
        ;; line with single spaces.
        (while (re-search-forward "\\(.\\)  " nil t)
          (replace-match (concat (match-string-no-properties 1) " ")))
        (goto-char (point-min))
        ;; Replace all remaining 2 spaces with '+' symbol.
        (while (search-forward "  " nil t)
          (replace-match "+" nil t))
        ;; Move page number to after + symbols.
        (goto-char (point-min))
        (while (re-search-forward " (\\([0-9]+\\))" nil t)
          (let ((page-number (match-string-no-properties 1)))
            (replace-match "")
            (beginning-of-line)
            (re-search-forward "\\(\\+*\\)" (line-end-position) t)
            (replace-match (concat (match-string-no-properties 1)
                                   page-number
                                   " "))))))))


;;; pdf-layer.el ends here
