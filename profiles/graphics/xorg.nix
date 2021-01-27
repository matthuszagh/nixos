{ config
, pkgs
, ...
}:
let
  useStartx = true;
in
{
  services.xserver = {
    # Enable touchpad support.
    libinput.enable = true;
    libinput.touchpad.tapping = false;
    libinput.touchpad.disableWhileTyping = true;

    enable = true;
    layout = "us";
    xkbOptions = "ctrl:swapcaps";

    enableCtrlAltBackspace = true;

    # remote connections
    enableTCP = true;

    displayManager =
      if useStartx then {
        startx.enable = true;
      } else {
        sddm.enable = true;
        plasma5.enable = true;
      };
  };

  environment.systemPackages = with pkgs; [
    plasma5Packages.breeze-gtk
    plasma5Packages.breeze-qt5
    plasma5Packages.kde-gtk-config
  ];
}
