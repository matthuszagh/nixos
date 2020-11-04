{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.glasgow;
in
{
  options = {
    hardware.glasgow = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to Glasgow.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # Glasgow
      ENV{ID_VENDOR_ID}=="20b7", ENV{ID_MODEL_ID}=="9db1", MODE:="666"
    '';
  };
}
