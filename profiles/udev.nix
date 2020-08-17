{ config, pkgs, ... }:

{
    services.udev = {
      packages = with pkgs; [ hackrf ];
      extraRules = ''
        # SuperLead 2300 QR scanner
        ACTION=="add", SUBSYSTEM=="input", ATTR{idVendor}=="2dd6", ATTR{idProduct}=="0260", MODE="0666"
        ACTION=="add", SUBSYSTEM=="input", \
          ENV{ID_SERIAL}=="SuperLead_2300_00000000", \
          ENV{ID_USB_INTERFACE_NUM}=="00", \
          SYMLINK+="teemi_scan"

        # Brother PT-1230PC label printer
        ACTION=="add", SUBSYSTEM=="usbmisc", \
          ATTR{idVendor}=="04f9", ATTR{idProduct}=="202c", MODE="0666"

        # Epson ES-400 scanner
        ENV{ID_VENDOR_ID}=="04b8", ENV{ID_MODEL_ID}=="0156", MODE:="666, GROUP="scanner"

        # Glasgow
        SUBSYSTEM=="usb", ATTRS{idVendor}=="20b7", ATTRS{idProduct}=="9db1", \
          MODE="0660", GROUP="plugdev", TAG+="uaccess"

        # Cypress (also for glasgow)
        SUBSYSTEM=="usb", ATTRS{idVendor}=="04b4", ATTRS{idProduct}=="8613", \
          MODE="0660", GROUP="plugdev", TAG+="uaccess"

        # FMCW Radar
        ENV{ID_VENDOR_ID}=="0403", ENV{ID_MODEL_ID}=="6010", MODE:="666"

        # RF Explorer
        ACTION=="add", \
        ENV{ID_SERIAL}=="Silicon_Labs_CP2102N_USB_to_UART_Bridge_Controller_e27ea5e3ff66e9119537f363867e4c5a", \
        MODE:="666"

        # NI gpib-usb-hs
        #SUBSYSTEM=="usb", ACTION=="add", ATTRS{idVendor}=="3923", ATTRS{idProduct}=="709b", \
        #  ATTRS{serial}=="01E01169", SYMLINK+="gpib0", MODE:="666"

        # Mayflash GC adapter
        SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="057e", \
          ATTRS{idProduct}=="0337", MODE="0666"
      '';
    };
}
