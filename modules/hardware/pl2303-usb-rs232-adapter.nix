{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.pl2303-usb-rs232-adapter;
in
{
  options = {
    hardware.pl2303-usb-rs232-adapter = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to PL2303 USB RS232 adapter.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # PL2303 USB RS232 adapter
      ENV{ID_VENDOR_ID}=="067b", ENV{ID_MODEL_ID}=="2303", MODE:="666"
    '';
  };
}
