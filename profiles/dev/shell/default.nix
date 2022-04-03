{ pkgs
, ...
}:

{
  imports = [
    ./bash.nix
    ./fish.nix
  ];

  environment.systemPackages = with pkgs; [
    less
    vim
    screen
  ];

  programs.thefuck.enable = true;
}
