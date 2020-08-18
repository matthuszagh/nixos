{ pkgs
, ...
}:

{
  environment.systemPackages = (with pkgs; [
    lm_sensors
    tlp
    powertop
  ]);
  services = {
    upower.enable = true;
    tlp = {
      enable = true;
      settings = {
        USB_BLACKLIST_PHONE = 1;
        CPU_HWP_ON_AC = "performance";
        CPU_HWP_ON_BAT = "power";
      };
    };
  };
}
