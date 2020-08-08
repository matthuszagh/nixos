{ config, pkgs, ... }:

let
  nix-tree = (builtins.fetchTarball {
    name = "nix-tree-unstable-2020-07-15";
    url = "https://github.com/utdemir/nix-tree/archive/0b7980778e622c83db1c46836cb41602e502b673.tar.gz";
    sha256 = "1a2in3qy9n39ipf008hq3b856l9f4labx7kbxvlvr5pvc8w38k02";
  });
in
{
  home-manager.users.matt = { pkgs, ... }: {
    nixpkgs.overlays = [
      (import "${nix-tree}/overlay.nix" {})
    ];

    home.packages = with pkgs; [
      nix-tree
    ];
  };
}
