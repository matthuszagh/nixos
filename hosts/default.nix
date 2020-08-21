{ utils
, lib
, pkgs
, system
, home
, self
}:

let
  inherit (utils) recImport;

  config = hostName:
  lib.nixosSystem {
    inherit system;
    inherit pkgs;

    modules = let
      inherit (home.nixosModules) home-manager;

      common = {
        imports = [
          ../profiles  # hardware-independent system configurations
          ../users
          ../modules
        ];

        hardware.enableRedistributableFirmware = lib.mkDefault true;

        networking.hostName = hostName;
        system.configurationRevision = lib.mkIf (self ? rev) self.rev;
        users.mutableUsers = false;

        nixpkgs.pkgs = pkgs;
      };

      machine = import "${toString ./.}/${hostName}.nix";
    in [
      common
      machine
      home-manager
    ];
  };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in
hosts
