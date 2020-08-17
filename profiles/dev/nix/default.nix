{ pkgs }:

{
  environment.systemPackages = with pkgs; [
    nixpkgs-fmt
  ]
}
