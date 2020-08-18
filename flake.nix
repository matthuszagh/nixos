{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    home.url = "github:rycee/home-manager/bqv-flakes";
  };

  outputs = { self, nixpkgs, home }:
  let
    inherit (builtins) attrValues;
    inherit (nixpkgs) lib;

    utils = import ./lib/utils.nix { inherit lib; };

    system = "x86_64-linux";

    pkgImport = pkgs: import pkgs {
      inherit system;
      overlays = attrValues self.overlays;
      config = { allowUnfree = true; };
    };

    pkgs = pkgImport nixpkgs;
  in
  {
    # Set of machine/build outputs, where key is the machine name and
    # the value defines the configuration of the machine.
    nixosConfigurations = import ./hosts {
      inherit lib pkgs system utils self home;
    };

    # devShell."${system}" = import ./shell.nix {
    #   inherit pkgs;
    # };
  };
}
