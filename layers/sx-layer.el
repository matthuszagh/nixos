;;; sx-layer.el --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(layer-def sx
  :presetup
  (:layer straight
   (straight-use-package 'sx))

  :setup
  (use-package sx)

  :postsetup
  (:layer modal
   (general-def 'normal sx-inbox-mode-map
     "RET" 'sx-display)
   (general-def mh/prefix-search-map
     "S" 'sx-search)
   (localleader :keymaps 'sx-question-mode-map
     "A" 'sx-accept
     "a" 'sx-answer
     "u" 'sx-upvote
     "d" 'sx-downvote)))

;;; sx-layer.el ends here
