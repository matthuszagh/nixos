{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.rioddas-cd-dvd-drive;
in
{
  options = {
    hardware.rioddas-cd-dvd-drive = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to Rioddas CD/DVD drive.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # Rioddas CD/DVD drive
      ENV{ID_VENDOR_ID}=="13fd", ENV{ID_MODEL_ID}=="0840", MODE:="666"
    '';
  };
}
