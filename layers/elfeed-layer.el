;;; elfeed-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def elfeed
  :setup
  (use-package elfeed
    :config
    (setq elfeed-feeds
          '(("http://nullprogram.com/feed/" emacs)
            ("http://endlessparentheses.com/atom.xml" emacs)
            ("https://www.preposterousuniverse.com/blog/feed/" physics)
            ("http://www.math.columbia.edu/~woit/wordpress/?feed=rss2" physics)
            ("https://lab.whitequark.org/atom.xml")
            ("http://newartisans.com/rss.xml" programming)))
    (setq elfeed-search-filter "@6-months-ago +unread"))

  :postsetup
  (:layer modal
   (general-def mh/prefix-search-map
     "b" 'elfeed)
   (localleader :keymaps 'elfeed-search-mode-map
     "u" 'elfeed-update
     "s" 'elfeed-search-live-filter)))

;;; elfeed-layer.el ends here
