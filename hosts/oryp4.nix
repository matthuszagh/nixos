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
          "cryptnvme".device = "/dev/disk/by-uuid/73c0dace-dd87-486e-9b5f-d0f9e60ae353";
          "cryptsd".device = "/dev/disk/by-uuid/f9bfd4e3-89da-434c-920b-9f646027cecd";
        };
      };
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/89519238-a940-4adb-a18e-2b5229ade6fa";
      fsType = "btrfs";
      options = [ "subvol=nixos" "ssd" "noatime" "compress=zstd:2" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/684A-B3D0";
      fsType = "vfat";
    };

  swapDevices = [ ];

  services.xserver = {
    videoDrivers = if useNvidia then [ "nvidia" ] else [ "intel" ];
    resolutions = [{ x = 3840; y = 2160; }];
    dpi = 192;
    defaultDepth = 24;
  };

  # Nvidia GPU must either be used in sync mode (always on with
  # proprietary Nvidia drivers), or with the open source Nvidia
  # drivers (nouveau). The Nvidia GPU cannot be fully turned off
  # because the GeForce 1070 uses the Pascal microarchitecture
  # (according to https://nixos.wiki/wiki/Nvidia). Therefore,
  # `useNvidia=true` fully enables the GPU, whereas `useNvidia=false`
  # use the nouveau driver for it. Unfortunately, this GPU cannot be
  # used in "offload" mode, which would enable turning it on and off
  # at will.
  hardware =
    if useNvidia then {
      nvidia = {
        modesetting.enable = true;
        prime = {
          sync.enable = true;
          nvidiaBusId = "PCI:1:0:0";
          intelBusId = "PCI:0:2:0";
        };
      };
      video.hidpi.enable = lib.mkDefault true;
    } else {
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

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
