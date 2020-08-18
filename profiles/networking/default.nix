{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    wpa_supplicant
  ];
}
