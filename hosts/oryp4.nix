{ lib
, config
, pkgs
, ...
}:
let
  useNvidia = false;
in
{
  imports = [
    ./profiles/systemd-boot.nix
    ./profiles/btrfs.nix
  ];

  boot = {
    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
      kernelModules = [ ];
      luks = {
        devices = {
          "cryptnvme".device = "/dev/disk/by-uuid/b2d421d4-5431-4887-9270-74d9a95b3b27";
          "cryptsd".device = "/dev/disk/by-uuid/4c98c733-a031-4a12-bfda-3f364130db00";
        };
      };
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/ff14b70d-390b-4c97-b49e-d7ea8eeee5bf";
      fsType = "btrfs";
      options = [ "subvol=nixos" "ssd" "noatime" "defaults" "compress=zstd:2" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/7654-76D2";
      fsType = "vfat";
    };

  swapDevices = [ ];

  services.xserver = {
    videoDrivers = if useNvidia then [ "nvidia" ] else [ "intel" ];
    resolutions = [{ x = 3840; y = 2160; }];
    dpi = 192;
    defaultDepth = 24;
  };

  hardware =
    if useNvidia then {
      nvidia = {
        modesetting.enable = true;
        prime = {
          sync.enable = true;
          # offload.enable = true;
          nvidiaBusId = "PCI:1:0:0";
          intelBusId = "PCI:0:2:0";
        };
      };
    } else {
      opengl.extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
      cpu.intel.updateMicrocode = true;
    };

  networking = {
    wireless = {
      enable = true;
      networks = import ../secrets/wifi.nix;
    };

    useDHCP = true;
    dhcpcd.persistent = true;
  };

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
