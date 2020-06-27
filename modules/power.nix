{ config, pkgs, ... }:

{
  environment.systemPackages = (with pkgs; [
    lm_sensors
  ]);
  services = {
    upower.enable = true;
    tlp = {
      enable = true;
      extraConfig = ''
        USB_BLACKLIST_PHONE=1
        CPU_HWP_ON_AC=performance
        CPU_HWP_ON_BAT=power
      '';
    };
  };
}
