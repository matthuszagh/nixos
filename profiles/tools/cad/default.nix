{ pkgs
, ...
}:
{
  environment.systemPackages = with pkgs; [
    freecad
    graphviz
    cura
  ];
}
