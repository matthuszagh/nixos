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
          ipython
          debugpy  # needed for DAP
        ];

      python = pkgs.python3.withPackages packages;

    in [
      python
    ] ++ (with pkgs; [
      black
      nodejs  # needed for DAP
    ]);
}
