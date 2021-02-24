{ pkgs
, ...
}:

{
  environment.systemPackages = (with pkgs; [
    lm_sensors
    tlp
    powertop
    # needed to disable wifi
    networkmanager
  ]);
  services = {
    upower.enable = true;
    tlp = {
      enable = true;
      settings = {
        USB_BLACKLIST_PHONE = 1;
        # CPU settings
        CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 100;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 30;
        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;
        # disable bluetooth and wifi on battery power
        DEVICES_TO_ENABLE_ON_AC = [ "bluetooth wifi" ];
        DEVICES_TO_DISABLE_ON_BAT = [ "bluetooth wifi" ];
        WOL_DISABLE = "N";
      };
    };
  };
}
