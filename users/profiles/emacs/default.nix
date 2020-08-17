# TODO using local files like this won't work with flakes. I believe
# I'd have to manage the emacs configuration in this repo in order for
# this to work. I'm not thrilled about that because although this
# depends on emacs, my emacs configuration does not depend on this.
let
  emacsSrcDir = "/home/matt/src/dotfiles/emacs";
in
{
  home.file = {
    ".config/emacs/init.el".source = "${emacsSrcDir}/init.el";
    ".config/emacs/.gnus.el".source = "${emacsSrcDir}/.gnus.el";
    ".config/emacs/layers" = {
      source = "${emacsSrcDir}/layers";
      recursive = true;
    };
    ".config/emacs/snippets" = {
      source = "${emacsSrcDir}/snippets";
      recursive = true;
    };
    ".config/emacs/scripts" = {
      source = "${emacsSrcDir}/scripts";
      recursive = true;
    };
  };
}
