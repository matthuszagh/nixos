{ pkgs
, ...
}:

{
  imports = [
    ./opengl.nix
    ./xorg.nix
  ];

  fonts = {
    fonts = with pkgs; [
      source-code-pro
    ];
  };

  environment.systemPackages = with pkgs; [
    imagemagick
    inkscape
    gimp
    ghostscript
    gnuplot
    vlc
    mpv
    kdeApplications.gwenview # TODO this will become plasma5Packages.gwenview in a future nixpkgs version
    mesa
    xlibs.xwininfo
    xlibs.xhost
    xlibs.xdpyinfo
    glxinfo
    gnome3.gnome-settings-daemon
    breeze-icons
    asymptote
    obs-studio
    ffmpeg
  ];
}
