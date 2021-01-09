{ pkgs
, ...
}:

{
  imports = [
    ./vpn/pia
    ./ssh.nix
    ./syncthing.nix
    ./torrent.nix
  ];

  environment.systemPackages = with pkgs; [
    wpa_supplicant
    speedtest-cli
    firefox
    google-chrome
    nyxt
    nethogs
  ];
}
