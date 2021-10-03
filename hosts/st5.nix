{ lib
, pkgs
, ...
}:

{
  imports = [
    ./profiles/systemd-boot.nix
    ./profiles/btrfs.nix
  ];

  boot = {
    initrd = {
      availableKernelModules = [ "xhci_pci" "nvme" "uas" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ "dm-snapshot" ];
      luks = {
        devices = {
          "cryptsd".device = "/dev/disk/by-uuid/ef495375-8e70-41ef-995f-46222eb53b24";
        };
      };
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/5477-51EA";
      fsType = "vfat";
    };

  fileSystems."/" =
    {
      device = "/dev/volgroup/nixos";
      fsType = "btrfs";
      options = [ "subvol=nixos" "ssd" "noatime" "defaults" "compress=zstd:2" ];
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services.xserver = {
    videoDrivers = [ "intel" ];
    resolutions = [{ x = 3840; y = 2160; }];
    dpi = 192;
    defaultDepth = 24;
  };

  hardware = {
    opengl.extraPackages = with pkgs; [
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
    ];
    cpu.intel.updateMicrocode = true;
    video.hidpi.enable = lib.mkDefault true;
  };

  networking = {
    wireless = {
      enable = true;
      networks = import ../secrets/wifi.nix;
    };

    useDHCP = true;
    dhcpcd.persistent = true;
  };
}
