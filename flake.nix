{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    home.url = "github:rycee/home-manager";
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    # package overrides
    sageNixpkgs.url = "github:nixos/nixpkgs/7866440f1223dc447f38cb23c00e10b44b4c98fe";
    paraviewNixpkgs.url = "github:nixos/nixpkgs/72158c231ae46a34ec16b8134d2a8598506acd9c";
    anystyleNixpkgs.url = "github:SCOTT-HAMILTON/nixpkgs/4cf6c95cb021b62e78e769af7ba64280b340b666";
    vivadoNixpkgs.url = "github:matthuszagh/nixpkgs/vivado";
    ladyHeatherNixpkgs.url = "github:matthuszagh/nixpkgs/lady-heather";
    verilatorNixpkgs.url = "github:nixos/nixpkgs/1e451da3860c1c562fdccefb7f0380f765251399";
    latexmlNixpkgs.url = "github:xworld21/nixpkgs/d66ad88b42ce0894d4c05b7f9fdb270d6c39df5f";
    koreaderNixpkgs.url = "github:contrun/nixpkgs/f899b94e7072d8c217aedcbf047d174a4530d4a1";
    ventoyNixpkgs.url = "github:Br1ght0ne/nixpkgs/f09655337b699bcc323e3b92a670d910e77035ae";
    # https://github.com/NixOS/nixpkgs/issues/94315#issuecomment-719892849
    mesaNixpkgs.url = "github:nixos/nixpkgs/bdac777becdbb8780c35be4f552c9d4518fe0bdb";
    asymptoteNixpkgs.url = "github:nixos/nixpkgs/6eeb612a9b6160df2e110021d93bef3e6e538b9a";
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
    , ladyHeatherNixpkgs
    , verilatorNixpkgs
    , latexmlNixpkgs
    , koreaderNixpkgs
    , ventoyNixpkgs
    , mesaNixpkgs
    , asymptoteNixpkgs
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
        lady-heather = (pkgImport ladyHeatherNixpkgs).lady-heather;
        verilator = (pkgImport verilatorNixpkgs).verilator;
        LaTeXML = (pkgImport latexmlNixpkgs).perlPackages.LaTeXML;
        koreader = (pkgImport koreaderNixpkgs).koreader;
        ventoy = (pkgImport ventoyNixpkgs).ventoy;
        mesa_drivers = (pkgImport mesaNixpkgs).mesa_drivers;
        asymptote = (pkgImport asymptoteNixpkgs).asymptote;
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
