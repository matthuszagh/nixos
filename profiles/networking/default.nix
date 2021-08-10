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
    wget
    wget2
    curl
    wpa_supplicant
    speedtest-cli
    firefox
    google-chrome
    nyxt
    nethogs
  ];
}
