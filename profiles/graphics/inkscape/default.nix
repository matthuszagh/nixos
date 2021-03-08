{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    inkscape
  ];

  home.file.".config/inkscape/symbols/electrical-engineering.svg".source = ./symbols/electrical-engineering.svg;
}
