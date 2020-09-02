{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    ngspice
    (kicad.override {
      symbol-path = "/home/matt/src/kicad-symbols";
      footprint-path = "/home/matt/src/kicad-footprints";
      model3d-path = "/home/matt/src/kicad-packages3D";
    })
  ];
}
