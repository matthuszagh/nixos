{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  name = "nixos";
  nativeBuildInputs = with pkgs; [
    git
    git-crypt
    nixFlakes
  ];
}
