{ pkgs
, ...
}:

{
  imports = [
    ./locate.nix
  ];

  environment.systemPackages = with pkgs; [
    # TODO fix
    # recoll
  ];
}
