{ config
, lib
, pkgs
, ...
}:

{
  imports =[
    ./profiles/systemd-boot.nix
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "uas" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "wl" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/9b9f4bd8-732c-42a0-aa48-140f638a6952";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/67E3-17ED";
      fsType = "vfat";
    };

  services.xserver = {
    videoDrivers = [ "intel" ];
    resolutions = [ { x = 2560; y = 1600; } ];
    dpi = 192;
    defaultDepth = 24;
  };

  environment.variables.XCURSOR_SIZE = "32";

  swapDevices = [ { device = "/swapfile"; size = 8192; } ];

  networking = {
    hostName = "mbp";
    wireless = {
      enable = true;
      networks = import ../secrets/wifi.nix;
    };

    useDHCP = true;
    dhcpcd.persistent = true;
  };

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
