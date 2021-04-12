{ pkgs
, ...
}:
{
  environment.systemPackages = with pkgs; [
    ngspice
    kicad
    lady-heather
  ];
}
