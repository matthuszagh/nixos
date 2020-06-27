{ useStartx ? true
, useNvidia ? false
, pkgs
, ... }:

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
    };

    desktopManager = if useStartx then {
    } else {
      plasma5.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    plasma5.breeze-gtk
    plasma5.breeze-qt5
    plasma5.kde-gtk-config
  ];

  home-manager.users.matt = { ... }: {
    home.file.".xinitrc".text = (if useNvidia then ''
      xrandr --setprovideroutputsource modesetting NVIDIA-0
      xrandr --auto
    '' else '''') + ''
      # Disable access control for the current user.
      xhost +SI:localuser:$USER

      # Set themes, etc.
      xrdb ~/.Xresources

      # Stop creating lots of serverauth files
      xserverauthfile=$XAUTHORITY

      # Make Java applications aware this is a non-reparenting window manager.
      export _JAVA_AWT_WM_NONREPARENTING=1

      # Set keyboard repeat rate.
      xset r rate 200 25

      # hidpi
      xrandr --dpi 192

      # always keep numlock on
      # numlockx &

      # Finally start Emacs
      exec emacs
      # exec emacsclient
    '';
    home.file.".Xresources".text = ''
      Xcursor.size: 32
      Xcursor.theme: breeze_cursors
    '';
  };
}
