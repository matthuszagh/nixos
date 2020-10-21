{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.stlink-v3;
in
{
  options = {
    hardware.stlink-v3 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to stlink v3.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # STLink V3
      ENV{ID_VENDOR_ID}=="0483", ENV{ID_MODEL_ID}=="374e", MODE:="666"
    '';
  };
}
