{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.ti866ii-plus;
in
{
  options = {
    hardware.ti866ii-plus = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to TI866II Plus Universal Programmer.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # TI866II Plus Universal Programmer
      ENV{ID_VENDOR_ID}=="a466", ENV{ID_MODEL_ID}=="0a53", MODE:="666"
    '';
  };
}
