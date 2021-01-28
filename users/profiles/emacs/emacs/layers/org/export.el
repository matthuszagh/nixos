;;; export.el --- Org Export -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ox
  :config
  ;; HTML supports 6 headline levels. Use all 6. The default only
  ;; presents to top 3 as full sections.
  (setq org-export-headline-levels 6))

(use-package ox-html
  :config
  ;; only wrap code elements in spans and use a CSS file to fontify elements.
  (setq org-html-htmlize-output-type 'css))

(use-package ox-latex
  :config
  (setq org-latex-compiler "lualatex"))

(use-package ox-publish
  :config
  (setq blog-dir "~/src/blog/")
  (setq org-publish-project-alist
        `(("blog" :components ("posts" "static"))
          ("posts"
           :base-directory ,(concat blog-dir "org")
           :publishing-directory ,blog-dir
           :publishing-function org-html-publish-to-html
           :htmlize-source t
           :recursive t
           :body-only t
           :with-toc nil
           :exclude "\\(old\\|purgatory\\)")
          ("static"
           :base-directory ,(concat blog-dir "org")
           :base-extension "svg\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|mathml"
           :publishing-directory ,blog-dir
           :publishing-function org-publish-attachment
           :exclude "\\(old\\|purgatory\\)"
           :recursive t))))

;;; export.el ends here
