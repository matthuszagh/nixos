{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.cp210x-uart-usb-bridge;
in
{
  options = {
    hardware.cp210x-uart-usb-bridge = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to CP210x UART USB bridge.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # CP210x UART USB bridge
      ENV{ID_VENDOR_ID}=="10c4", ENV{ID_MODEL_ID}=="ea60", MODE:="666"
    '';
  };
}
