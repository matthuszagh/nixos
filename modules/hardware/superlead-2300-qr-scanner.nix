{ config
, pkgs
, lib
, ...
}:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.superlead-2300-qr-scanner;
in
{
  options = {
    hardware.superlead-2300-qr-scanner = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to symlink SuperLead 2300 QR scanner under /dev.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # SuperLead 2300 QR scanner
      ACTION=="add", SUBSYSTEM=="input", ATTR{idVendor}=="2dd6", ATTR{idProduct}=="0260", MODE="0666"
      ACTION=="add", SUBSYSTEM=="input", \
        ENV{ID_SERIAL}=="SuperLead_2300_00000000", \
        ENV{ID_USB_INTERFACE_NUM}=="00", \
        SYMLINK+="teemi_scan"
    '';
  };
}
