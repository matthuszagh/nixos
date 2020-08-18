{ pkgs, ... }:

{
  nix = {
    package = pkgs.nixFlakes;

    # disallows dependencies outside the nix store.
    useSandbox = true;
    nrBuildUsers = 100;

    # nix-command uses a new command interface for nix based on
    # flakes. flakes enables flakes support
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';

    # hardlink identical files in the store to save disk space
    autoOptimiseStore = true;

    # automatically run nix store optimiser
    optimise.automatic = true;

    # automatically garbage-collect the nix store
    gc.automatic = true;

    # Users allowed to connect to the nix daemon. The `@` symbol means
    # all users in that group.
    allowedUsers = [ "@wheel" ];

    # Users with additional permissions when connecting to the nix
    # daemon, such as specifying additional binary caches. The `@`
    # symbol means all users in that group.
    trustedUsers = [ "root" "@wheel" ];
  };

  nixpkgs.config.allowUnfree = true;
}
