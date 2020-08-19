{
  imports = [
    ./matt.nix
    ./root.nix
  ];

  # Uses system packages for home-manager. This allows us to use all
  # overlays in home-manager.
  home-manager.useGlobalPkgs = true;
}
