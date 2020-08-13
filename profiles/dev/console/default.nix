{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    less
    vim
  ];

  programs.thefuck.enable = true;
}
