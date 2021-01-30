{
  home.file = {
    ".config/emacs/init.el".source = ./emacs/init.el;
    ".config/emacs/.gnus.el".source = ./emacs/.gnus.el;
    ".config/emacs/layers" = {
      source = ./emacs/layers;
      recursive = true;
    };
    ".config/emacs/snippets" = {
      source = ./emacs/snippets;
      recursive = true;
    };
    ".config/emacs/scripts" = {
      source = ./emacs/scripts;
      recursive = true;
    };
    ".config/emacs/straight/versions/default.el".source = ./emacs/straight/versions/default.el;
  };
}
