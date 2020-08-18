{
  imports = [
    ./db
    ./dev
    ./games
    ./graphics
    ./networking
    ./mail
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
  };
}
