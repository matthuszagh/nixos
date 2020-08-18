{ config
, pkgs
, lib
, ...
}:

let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.fmcw-radar;
in
{
  options = {
    hardware.fmcw-radar = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to FMCW radar.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # FMCW Radar
      ENV{ID_VENDOR_ID}=="0403", ENV{ID_MODEL_ID}=="6010", MODE:="666"
    '';
  };
}
