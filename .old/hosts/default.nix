{ home
, lib
, pkgs
, self
, system
, utils
, ...
}:
let
  inherit (utils) recImport;
  inherit (builtins) attrValues removeAttrs;

  config = hostName:
    lib.nixosSystem {
      inherit system;

      modules =
        let
          inherit (home.nixosModules) home-manager;

          global = {
            imports = [
              ../local/locale.nix
              ../profiles/linux.nix
              ../profiles/nix.nix
            ];

            hardware.enableRedistributableFirmware = lib.mkDefault true;

            networking.hostName = hostName;
            nix.nixPath = let path = toString ../.; in
              [
                "nixpkgs=${pkgs}"
                "nixos=${pkgs}"
                "nixos-config=${path}/configuration.nix"
                "nixpkgs-overlays=${path}/overlays"
              ];

            nixpkgs = { pkgs = pkgs; };

            nix.registry = {
              nixpkgs.flake = pkgs;
            };

            system.configurationRevision = lib.mkIf (self ? rev) self.rev;

            # set all passwords declaratively
            users.mutableUsers = false;
          };

          # overrides = {
          #   # use latest systemd
          #   systemd.package = pkgs.systemd;

          #   nixpkgs.overlays =
          #     let
          #       override = import ../pkgs/override.nix pkgs;

          #       overlay = pkg: final: prev: {
          #         "${pkg.pname}" = pkg;
          #       };
          #     in
          #     map overlay override;
          # };

          local = import "${toString ./.}/${hostName}.nix";

          # Everything in `./modules/list.nix`.
          flakeModules =
            attrValues (removeAttrs self.nixosModules [ "profiles" ]);

        in
        flakeModules ++ [ global local home-manager ];

    };

  hosts = recImport {
    dir = ./.;
    _import = config;
  };
in
hosts
