{ pkgs
, ...
}:

{
  home.file.".config/inkscape/symbols/ee.svg".source = ./symbols/ee.svg;
  home.file.".config/inkscape/templates/default.svg".source = ./templates/default.svg;
}
