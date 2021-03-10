{ pkgs
, ...
}:

{
  hardware = {
    pulseaudio = {
      enable = true;
      support32Bit = true;
      extraModules = with pkgs; [
        pulseaudio-modules-bt
      ];
      package = pkgs.pulseaudioFull;
    };

    bluetooth = {
      enable = true;
      hsphfpd.enable = true;
      powerOnBoot = true;
      package = pkgs.bluezFull;
    };
  };

  services.blueman.enable = true;

  sound.enable = true;
}
