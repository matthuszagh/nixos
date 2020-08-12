{ pkgs, ... }:

let
  emacsEnv = (pkgs.emacsPackagesFor (pkgs.emacs)).emacsWithPackages
  (epkgs: (with epkgs.elpaPackages; [
  ]) ++ (with epkgs.melpaPackages; [
    vterm
    lsp-python-ms  # microsoft python language server
  ]) ++ (with epkgs.orgPackages; [
  ]) ++ (with epkgs; [
    pdf-tools
  ]));
in
{
  nixpkgs.overlays = [
    # TODO i believe there is an easier way to do this with flakes
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/ec96633b92c01d3ba78c5c5320222c75455bce17.tar.gz;
    }))
    (import ./overlay.nix)
  ];

  environment.systemPackages = with pkgs; [
    emacsEnv
  ];
}
