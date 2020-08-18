{ ... }:

{
  imports = [
    ./brother-pt1230-pc-label-printer.nix
    ./epson-es400-scanner.nix
    ./glasgow.nix
    ./superlead-2300-qr-scanner.nix
    ./fmcw-radar.nix
    # ./ni-gpib-usb-hs.nix
    ./mayflash-gc-adapter.nix
  ];
}
