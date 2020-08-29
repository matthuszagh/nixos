{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    home.url = "github:rycee/home-manager/bqv-flakes";
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    # package overrides
    sageNixpkgs.url = "github:nixos/nixpkgs/7866440f1223dc447f38cb23c00e10b44b4c98fe";
    paraviewNixpkgs.url = "github:nixos/nixpkgs/72158c231ae46a34ec16b8134d2a8598506acd9c";
    anystyleNixpkgs.url = "github:SCOTT-HAMILTON/nixpkgs/4cf6c95cb021b62e78e769af7ba64280b340b666";
    vivadoNixpkgs.url = "github:matthuszagh/nixpkgs/vivado";
  };

  outputs =
    { self
    , nixpkgs
    , home
    , emacsOverlay
    , sageNixpkgs
    , paraviewNixpkgs
    , anystyleNixpkgs
    , vivadoNixpkgs
    }:
    let
      inherit (builtins) attrNames attrValues readDir;
      inherit (nixpkgs) lib;
      inherit (lib) recursiveUpdate genAttrs mapAttrs' nameValuePair;
      inherit (utils) pathsToImportedAttrs;

      utils = import ./lib/utils.nix { inherit lib; };

      system = "x86_64-linux";

      externalOverlays = [
        emacsOverlay.overlay
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

      overridePkgs = {
        sageWithDoc = (pkgImport sageNixpkgs).sageWithDoc;
        paraview = (pkgImport paraviewNixpkgs).paraview;
        anystyle-cli = (pkgImport anystyleNixpkgs).anystyle-cli;
        vivado = (pkgImport vivadoNixpkgs).vivado;
      };

      pkgs = (pkgImport nixpkgs) // overridePkgs;
    in
    {
      # Set of machine/build outputs, where key is the machine name and
      # the value defines the configuration of the machine.
      nixosConfigurations = import ./hosts {
        inherit utils lib pkgs system home self;
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

      packages."${system}" = pkgs;

      # nixosModules = pathsToImportedAttrs (import ./modules/list.nix);
    };
}
