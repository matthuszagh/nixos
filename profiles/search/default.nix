{ pkgs
, ...
}:

{
  imports = [
    ./locate.nix
  ];

  environment.systemPackages = with pkgs; [
    recoll
  ];
}
