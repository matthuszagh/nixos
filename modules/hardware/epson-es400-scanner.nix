{ config
, pkgs
, lib
, ...
}:
let
  inherit (lib) mkOption mkIf types;
  cfg = config.hardware.epson-es400-scanner;
in
{
  options = {
    hardware.epson-es400-scanner = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Add udev rules to allow unprivileged access to Epson ES400 printer.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    services.udev.extraRules = ''
      # Epson ES-400 scanner
      ENV{ID_VENDOR_ID}=="04b8", ENV{ID_MODEL_ID}=="0156", MODE:="666"
    '';

    environment.systemPackages = with pkgs; [
      utsushi
      sane-backends
    ];
  };
}
