{ config, lib, pkgs, ... }:

let
  useNvidia = false;
  useStartx = true;
  modules-path = /etc/nixos/modules;
in
{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    (import (modules-path + "/xorg.nix") ({
      useStartx = useStartx;
      useNvidia = useNvidia;
      inherit pkgs;
    }))
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/7e553741-ca17-43f2-994d-da6d7f8a92ef";
      fsType = "btrfs";
      options = [ "subvol=nixos" "ssd" "noatime" "defaults" "compress=lzo"];
    };

  boot.initrd.luks.devices."cryptnvme".device = "/dev/disk/by-uuid/de6dd707-da33-4eaa-9a07-6dba6783f044";
  boot.initrd.luks.devices."cryptsd".device = "/dev/disk/by-uuid/a41ef7e1-ee7f-4eef-9a10-bb05775a5dba";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/9CC4-F224";
      fsType = "vfat";
    };

  swapDevices = [ ];

  services.xserver = {
    videoDrivers = if useNvidia then [ "nvidia" ] else [ "intel" ];
    resolutions = [ { x = 3840; y = 2160; } ];
    dpi = 192;
    defaultDepth = 24;
  };

  hardware = if useNvidia then {
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
    hostName = "oryp4";
    wireless = {
      enable = true;
      networks = import ./security/wifi.nix;
      # specifying wifi networks follows the following syntax:
      # the available network with the highest priority is connected to.
      # networks = {
      #   "network name 1" = {
      #     psk = "password1";
      #     priority = 100;
      #   };
      #   "network name 2" = {
      #     psk = "password2";
      #     priority = 50;
      #   };
      #   ...
      # };
    };

    useDHCP = true;
    dhcpcd.persistent = true;
  };

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
