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
    libinput.tapping = false;
    libinput.disableWhileTyping = true;

    enable = true;
    layout = "us";
    xkbOptions = "ctrl:swapcaps";

    enableCtrlAltBackspace = true;

    # remote connections
    enableTCP = true;

    displayManager = if useStartx then {
      startx.enable = true;
    } else {
      sddm.enable = true;
      plasma5.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    plasma5.breeze-gtk
    plasma5.breeze-qt5
    plasma5.kde-gtk-config
  ];
}
