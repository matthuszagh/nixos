{ pkgs
, ...
}:
let
  pythonEnv = pkgs.python3.withPackages (p: with p; [
    pyclipper
  ]);
in
{
  environment.systemPackages = with pkgs; [
    ngspice
    (kicad.override {
      symbol-path = "/home/matt/src/kicad-symbols";
      footprint-path = "/home/matt/src/kicad-footprints";
      model3d-path = "/home/matt/src/kicad-packages3D";
    })
    pythonEnv
  ];
}
