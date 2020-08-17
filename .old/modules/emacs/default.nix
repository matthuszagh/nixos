{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # utilities
    ispell
    ghostscript
    languagetool

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
