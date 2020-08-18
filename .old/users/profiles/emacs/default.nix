{
  home.file = {
    ".config/emacs/init.el".source = ./init.el;
    ".config/emacs/.gnus.el".source = ./.gnus.el;
    ".config/emacs/layers" = {
      source = ./layers;
      recursive = true;
    };
    ".config/emacs/snippets" = {
      source = ./snippets;
      recursive = true;
    };
    ".config/emacs/scripts" = {
      source = ./scripts;
      recursive = true;
    };
  };
}
