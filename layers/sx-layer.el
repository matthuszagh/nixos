;;; sx-layer.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Code:

(layer-def sx
  :setup
  (use-package sx)

  :postsetup
  (:layer modal
   (general-def 'normal sx-inbox-mode-map
     "RET" 'sx-display)
   (general-def mh/prefix-prog-map
     "e" 'sx-inbox)
   (general-def mh/prefix-search-map
     "S" 'sx-search)
   (localleader :keymaps 'sx-question-mode-map
     "A" 'sx-accept
     "a" 'sx-answer
     "u" 'sx-upvote
     "d" 'sx-downvote)))

;;; sx-layer.el ends here
