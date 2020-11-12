{
  imports = [
    ./db
    ./dev
    ./games
    ./graphics
    ./networking
    ./mail
    ./messaging
    ./search
    ./security
    ./system
    ./tools
    ./virtualization.nix
    ./writing
  ];

  boot.cleanTmpDir = true;

  hardware = {
    superlead-2300-qr-scanner.enable = true;
    brother-pt1230-pc-label-printer.enable = true;
    epson-es400-scanner.enable = true;
    glasgow.enable = true;
    fmcw-radar.enable = true;
    ni-gpib-usb-hs.enable = true;
    stlink-v3.enable = true;
  };
}
