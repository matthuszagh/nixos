{ config
, pkgs
, lib
, ...
}:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.brother-pt1230-pc-label-printer;
in
{
  options = {
    hardware.brother-pt1230-pc-label-printer = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to Brother PT-1230 label printer.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # Brother PT-1230PC label printer
      ACTION=="add", SUBSYSTEM=="usbmisc", \
        ATTR{idVendor}=="04f9", ATTR{idProduct}=="202c", MODE="0666"
    '';
  };
}
