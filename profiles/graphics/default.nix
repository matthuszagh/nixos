{ pkgs
, ...
}:

{
  imports = [
    ./opengl.nix
  ];

  fonts = {
    fonts = with pkgs; [
      source-code-pro
    ];
  };

  environment.systemPackages = with pkgs; [
    imagemagick
    gimp
    inkscape
  ];
}
