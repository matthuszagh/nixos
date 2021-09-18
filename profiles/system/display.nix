{ pkgs
, ...
}:

{
  programs.light.enable = true;

  # relies on `location.provider = "geoclue2";` being set
  services.redshift = {
    enable = true;
    temperature = {
      day = 5500;
      night = 3700;
    };
    brightness = {
      day = "1";
      night = "0.5";
    };
  };
}
