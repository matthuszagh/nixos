{ config
, pkgs
, lib
, ...
}:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.mayflash-gc-adapter;
in
{
  options = {
    hardware.mayflash-gc-adapter = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to National Instruments GPIB USB HS.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # Mayflash GC adapter
      SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="057e", \
        ATTRS{idProduct}=="0337", MODE="0666"
    '';
  };
}
