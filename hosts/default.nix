{ utils
, lib
, system
, home
, ...
}:

let
  inherit (utils) recImport;

  config = hostName:
  lib.nixosSystem {
    inherit system;

    modules = let
      inherit (home.nixosModules) home-manager;

      local = import "${toString ./.}/${hostName}.nix";
    in
    [ local home-manager ];
  };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in
hosts
