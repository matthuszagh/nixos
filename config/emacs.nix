{ pkgs, ... }:

let custompkgs = import <custompkgs> {};
in
{
  home.file.".emacs.d/init.el".source = /home/matt/src/dotfiles/config/emacs/init.el;
  home.file.".emacs.d/layers".source = /home/matt/src/dotfiles/config/emacs/layers;
  home.file.".emacs.d/layers".recursive = true;
  home.file.".emacs.d/snippets".source = /home/matt/src/dotfiles/config/emacs/snippets;
  home.file.".emacs.d/snippets".recursive = true;
  home.file.".emacs.d/scripts".source = /home/matt/src/dotfiles/config/emacs/scripts;
  home.file.".emacs.d/scripts".recursive = true;
}
