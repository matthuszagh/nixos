{ config, pkgs, ... }:
let
  custompkgs = import <custompkgs> { };

  emacsSrcDir = "/home/matt/src/dotfiles/emacs";

  emacsEnv = (pkgs.emacsPackagesFor (pkgs.my-emacs)).emacsWithPackages
  (epkgs: (with epkgs.elpaPackages; [
  ]) ++ (with epkgs.melpaPackages; [
    vterm
    lsp-python-ms # microsoft python language server
  ]) ++ (with epkgs.orgPackages; [
  ]) ++ (with epkgs; [
    pdf-tools
  ]) ++ (with custompkgs; [
    org-recoll
  ]));
in
{
  home-manager.users.matt = { ... }: {
    home.file = {
      ".config/emacs/init.el".source = "${emacsSrcDir}/init.el";
      ".config/emacs/.gnus.el".source = "${emacsSrcDir}/.gnus.el";
      ".config/emacs/layers".source = "${emacsSrcDir}/layers";
      ".config/emacs/layers".recursive = true;
      ".config/emacs/snippets".source = "${emacsSrcDir}/snippets";
      ".config/emacs/snippets".recursive = true;
      ".config/emacs/scripts".source = "${emacsSrcDir}/scripts";
      ".config/emacs/scripts".recursive = true;
    };
    programs.emacs = {
      enable = true;
      package = emacsEnv;
    };
    # services.emacs = {
    #   enable = true;
    # };
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/ec96633b92c01d3ba78c5c5320222c75455bce17.tar.gz;
    }))
    (import ./overlay.nix)
  ];

  environment.systemPackages = with pkgs; [
    # TODO
    # rustc
    # rls
    rustfmt
    cargo
    # ============================== nix =============================
    nixpkgs-fmt

    ## language servers
    lua53Packages.digestif
    shellcheck
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodePackages.typescript
    python-language-server
    # TODO add fortran-language-server (github: hansec/fortran-language-server) to nixpkgs

    # search
    ripgrep
    # TODO fix
    # recoll

    # math / science
    # circuit simulation
    ngspice

    # utilities
    imagemagick
    ispell
    ghostscript
    languagetool
    gimp
    inkscape  # convert PDF to SVG

    # needed for edbi
    # perl-with-packages
    perlPackages.RPCEPCService
    perlPackages.DBI
    perlPackages.DBDPg
    perlPackages.LaTeXML

    # GUI
    #
    # Graphical applications are bundled with Emacs because I use
    # Emacs as my window manager. If a new graphical environment
    # is setup, such as a desktop manager, these programs can be
    # copied there as well.
    gsettings-desktop-schemas # needed with next
    # next-gtk-webkit
    anki
    vlc
  ] ++ (with custompkgs; [
    sbcl
    brainworkshop
    gnucap
  ]);
}
