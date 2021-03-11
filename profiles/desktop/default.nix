{ pkgs
, ...
}:

{
  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    gnome3.adwaita-icon-theme
    gnomeExtensions.appindicator
  ];

  services.udev.packages = with pkgs; [
    gnome3.gnome-settings-daemon
  ];
}
