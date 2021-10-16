{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/7e9b0dff974c89e070da1ad85713ff3c20b0ca97";
    home.url = "github:rycee/home-manager/release-21.05";
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    ## package overrides
    # 2021-04-07
    sageNixpkgs.url = "github:nixos/nixpkgs/49cfaef0c34007ade99c7a23497bd5993b8152f0";
    paraviewNixpkgs.url = "github:nixos/nixpkgs/72158c231ae46a34ec16b8134d2a8598506acd9c";
    vivadoNixpkgs.url = "github:matthuszagh/nixpkgs/vivado";
    ladyHeatherNixpkgs.url = "github:matthuszagh/nixpkgs/lady-heather";
    verilatorNixpkgs.url = "github:nixos/nixpkgs/1e451da3860c1c562fdccefb7f0380f765251399";
    dsviewNixpkgs.url = "github:nixos/nixpkgs/00b385c61226a3f3c396581fd7b4851d99e20910";
    inkscapeNixpkgs.url = "github:nixos/nixpkgs/797f77ab66f17c0c7e8c10b137a213d56c9ff36d";
    # https://github.com/NixOS/nixpkgs/issues/94315#issuecomment-719892849
    mesaNixpkgs.url = "github:nixos/nixpkgs/bdac777becdbb8780c35be4f552c9d4518fe0bdb";
    asymptoteNixpkgs.url = "github:nixos/nixpkgs/6eeb612a9b6160df2e110021d93bef3e6e538b9a";
    curaNixpkgs.url = "github:nixos/nixpkgs/797f77ab66f17c0c7e8c10b137a213d56c9ff36d";
    freecadNixpkgs.url = "github:nixos/nixpkgs/797f77ab66f17c0c7e8c10b137a213d56c9ff36d";
  };

  outputs =
    { self
    , nixpkgs
    , home
    , emacsOverlay
    , sageNixpkgs
    , paraviewNixpkgs
    , vivadoNixpkgs
    , ladyHeatherNixpkgs
    , verilatorNixpkgs
    , inkscapeNixpkgs
    , dsviewNixpkgs
    , mesaNixpkgs
    , asymptoteNixpkgs
    , curaNixpkgs
    , freecadNixpkgs
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
        config = {
          allowUnfree = true;
          permittedInsecurePackages = [
            "openssh-with-hpn-8.4p1"
          ];
        };
      };

      overridePkgs = {
        sageWithDoc = (pkgImport sageNixpkgs).sageWithDoc;
        paraview = (pkgImport paraviewNixpkgs).paraview;
        vivado = (pkgImport vivadoNixpkgs).vivado;
        lady-heather = (pkgImport ladyHeatherNixpkgs).lady-heather;
        verilator = (pkgImport verilatorNixpkgs).verilator;
        dsview = (pkgImport dsviewNixpkgs).dsview;
        inkscape = (pkgImport inkscapeNixpkgs).inkscape;
        mesa_drivers = (pkgImport mesaNixpkgs).mesa_drivers;
        asymptote = (pkgImport asymptoteNixpkgs).asymptote;
        cura = (pkgImport curaNixpkgs).cura;
        freecad = (pkgImport freecadNixpkgs).freecad;
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
