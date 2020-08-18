{ config
, pkgs
, lib
, ...
}:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.glasgow;
in
{
  options = {
    hardware.glasgow = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to Glasgow.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # Glasgow
      SUBSYSTEM=="usb", ATTRS{idVendor}=="20b7", ATTRS{idProduct}=="9db1", \
        MODE="0660", GROUP="plugdev", TAG+="uaccess"

      # Cypress (also for glasgow)
      SUBSYSTEM=="usb", ATTRS{idVendor}=="04b4", ATTRS{idProduct}=="8613", \
        MODE="0660", GROUP="plugdev", TAG+="uaccess"
    '';
  };
}
