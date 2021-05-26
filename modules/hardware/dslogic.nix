{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.dslogic;
in
{
  options = {
    hardware.dslogic = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to DSLogic Pro.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # DSLogic Pro
      ENV{ID_VENDOR_ID}=="2a0e", ENV{ID_MODEL_ID}=="002a", MODE:="666"
    '';
  };
}
