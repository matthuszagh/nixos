{ pkgs, ... }:
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
        ];

      python = pkgs.python3.withPackages packages;

    in
    [ python ];
}
