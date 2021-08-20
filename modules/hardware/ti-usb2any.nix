{ config
, pkgs
, lib
, ...
}:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.ti-usb2any;
in
{
  options = {
    hardware.ti-usb2any = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to TI USB2ANY.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # TI USB2ANY
      ENV{ID_VENDOR_ID}=="2047", ENV{ID_MODEL_ID}=="0301", MODE:="666"
      ENV{ID_VENDOR_ID}=="2047", ENV{ID_MODEL_ID}=="0200", MODE:="666"
    '';
  };
}
