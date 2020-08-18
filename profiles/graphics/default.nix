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
    ghostscript
    vlc
    mpv
    mesa
    xlibs.xwininfo
    xlibs.xhost
    xlibs.xdpyinfo
    glxinfo
    gnome3.gnome-settings-daemon
    breeze-icons
    asymptote
  ];
}
