{ pkgs }:

{
  environment.systemPackages = with pkgs; [
    nixpkgs-fmt
    nox
    nix-review
  ];
}
