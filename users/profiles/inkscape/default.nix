{ pkgs
, ...
}:

{
  home.file.".config/inkscape/symbols/electrical-engineering.svg".source = ./symbols/electrical-engineering.svg;
  home.file.".config/inkscape/templates/default.svg".source = ./templates/default.svg;
}
