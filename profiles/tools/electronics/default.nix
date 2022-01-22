{ pkgs
, ...
}:
{
  environment.systemPackages = with pkgs; [
    ngspice
    kicad
    lady-heather
    dsview
    linux-gpib
  ];
}
