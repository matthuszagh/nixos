{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    home.url = "github:rycee/home-manager/bqv-flakes";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, home, emacs-overlay }:
  let
    inherit (builtins) attrNames attrValues readDir;
    inherit (nixpkgs) lib;
    inherit (lib) recursiveUpdate genAttrs;
    inherit (utils) pathsToImportedAttrs;

    utils = import ./lib/utils.nix { inherit lib; };

    system = "x86_64-linux";

    externalOverlays = [
      emacs-overlay.overlay
    ];

    pkgImport = pkgs: import pkgs {
      inherit system;
      # Order is significant. First incorporate external overlays. We
      # can then use and override these in custom packages (defined
      # in `self.overlay`). Finally overlays in ./overlays can
      # override everything else.
      overlays = externalOverlays
        ++ [ self.overlay ]
        ++ (attrValues self.overlays);
      config = { allowUnfree = true; };
    };

    pkgs = pkgImport nixpkgs;
  in {
    # Set of machine/build outputs, where key is the machine name and
    # the value defines the configuration of the machine.
    nixosConfigurations = import ./hosts {
      inherit lib pkgs system utils self home;
    };

    overlay = import ./pkgs;

    overlays =
      let
        overlayDir = ./overlays;
        fullPath = name: overlayDir + "/${name}";
        overlayPaths = map fullPath (attrNames (readDir overlayDir));
      in
      pathsToImportedAttrs overlayPaths;

    devShell."${system}" = import ./shell.nix {
      inherit pkgs;
    };

    # packages."${system}" =
    #   let
    #     packages = self.overlay pkgs pkgs;
    #     overlayPkgs =
    #       genAttrs
    #         (attrNames self.overlays)
    #         (name: (self.overlays."${name}" pkgs pkgs)."${name}");
    #   in
    #   recursiveUpdate packages overlayPkgs;

    # nixosModules = pathsToImportedAttrs (import ./modules/list.nix);
  };
}
