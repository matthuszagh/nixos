{
  inputs =
    {
      master.url = "nixpkgs/master";
      nixos.url = "nixpkgs/release-20.03";
      openems.url = "github:matthuszagh/nixpkgs/openems";
      home.url = "github:rycee/home-manager/bqv-flakes";
    };

  outputs = inputs@{ self, home, nixos, openems, master }:
    let
      inherit (builtins) attrNames attrValues readDir;
      inherit (nixos) lib;
      inherit (lib) removeSuffix recursiveUpdate genAttrs filterAttrs;
      inherit (utils) pathsToImportedAttrs;

      utils = import ./lib/utils.nix { inherit lib; };

      system = "x86_64-linux";

      pkgImport = pkgs:
        import pkgs {
          inherit system;
          overlays = attrValues self.overlays;
          config = { allowUnfree = true; };
        };

      pkgset = {
        osPkgs = pkgImport nixos;
        pkgs = pkgImport master;
        openemsPkgs = pkgImport openems;
      };

    in
    with pkgset;
    {
      nixosConfigurations =
        import ./hosts (recursiveUpdate inputs {
          inherit lib pkgset system utils;
        }
        );

      devShell."${system}" = import ./shell.nix {
        inherit pkgs;
      };

      overlay = import ./pkgs;

      overlays =
        let
          overlayDir = ./overlays;
          fullPath = name: overlayDir + "/${name}";
          overlayPaths = map fullPath (attrNames (readDir overlayDir));
        in
        pathsToImportedAttrs overlayPaths;

      packages."${system}" =
        let
          packages = self.overlay osPkgs osPkgs;
          overlays = lib.filterAttrs (n: v: n != "pkgs") self.overlays;
          overlayPkgs =
            genAttrs
              (attrNames overlays)
              (name: (overlays."${name}" osPkgs osPkgs)."${name}");
        in
        recursiveUpdate packages overlayPkgs;

      nixosModules =
        let
          # binary cache
          cachix = import ./cachix.nix;
          cachixAttrs = { inherit cachix; };

          # modules
          moduleList = import ./modules/list.nix;
          modulesAttrs = pathsToImportedAttrs moduleList;

          # profiles
          profilesList = import ./profiles/list.nix;
          profilesAttrs = { profiles = pathsToImportedAttrs profilesList; };

        in
        recursiveUpdate
          (recursiveUpdate cachixAttrs modulesAttrs)
          profilesAttrs;
    };
}