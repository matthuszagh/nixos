{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.hp-officejet-pro-8600-plus-scanner;
in
{
  options = {
    hardware.hp-officejet-pro-8600-plus-scanner = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to HP OfficeJet Pro 8600 Plus scanner.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # HP OfficeJet Pro 8600 Plus scanner
      ENV{ID_VENDOR_ID}=="03f0", ENV{ID_MODEL_ID}=="5912", MODE:="666"
    '';

    environment.systemPackages = with pkgs; [
      utsushi
      sane-backends
    ];
  };
}
