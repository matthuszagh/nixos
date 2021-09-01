{
  imports = [
    ./db
    ./desktop
    ./dev
    ./finance
    ./fonts
    ./games
    ./graphics
    ./networking
    ./mail
    ./media
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
    dslogic.enable = true;
    epson-es400-scanner.enable = true;
    glasgow.enable = true;
    hp-officejet-pro-8600-plus-scanner.enable = true;
    fmcw-radar.enable = true;
    ni-gpib-usb-hs.enable = true;
    pl2303-usb-rs232-adapter.enable = true;
    rioddas-cd-dvd-drive.enable = true;
    stlink-v3.enable = true;
    ti866ii-plus.enable = true;
    ti-usb2any.enable = true;
  };
}
