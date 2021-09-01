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
  ];

  programs.thefuck.enable = true;
}
