{ pkgs
, ...
}:

{
  environment.systemPackages = (with pkgs; [
    pythonEnv
    mach-nix
    black
    nodejs # needed for DAP
    nodePackages.pyright # needed for lsp-pyright
  ]);
}
