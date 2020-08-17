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
}
