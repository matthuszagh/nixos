{ pkgs
, ...
}:
let inherit (pkgs) python3Packages;
in
{
  environment.systemPackages =
    let
      packages = pythonPackages:
        with pythonPackages; [
          numpy
          scipy
          matplotlib
          ipython
          debugpy # needed for DAP
          # TODO it's annoying to have to place pyclipper here,
          # instead of in the electronics/default.nix file
          pyclipper # needed for kicad plugin
          skidl
          pip
        ];

      pythonEnv = pkgs.python3.withPackages packages;

    in
    [
      pythonEnv
    ] ++ (with pkgs; [
      black
      nodejs # needed for DAP
      nodePackages.pyright # needed for lsp-pyright
    ]);
}
