{ config
, pkgs
, lib
, ...
}:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.ni-gpib-usb-hs;
in
{
  options = {
    hardware.ni-gpib-usb-hs = {
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
      # NI gpib-usb-hs
      SUBSYSTEM=="usb", ACTION=="add", ATTRS{idVendor}=="3923", ATTRS{idProduct}=="709b", \
        ATTRS{serial}=="01E01169", SYMLINK+="gpib0", MODE:="666"
    '';
  };
}
