{ config, lib, pkgs, ... }:

let
  useStartx = true;
  modules-path = /etc/nixos/modules;
  services-path = /etc/nixos/services;
in
{
  imports =[
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    (services-path + "/system/btrfs-backup.nix")
    (import (modules-path + "/xorg.nix") ({
      useStartx = useStartx;
      useNvidia = false;
      inherit pkgs;
    }))
  ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  hardware.cpu.amd.updateMicrocode = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/d31878d6-3a77-4f0f-9fdd-bb9a2c4e578b";
      fsType = "btrfs";
      options = [ "subvol=nixos" "compress=lzo" "ssd" "noatime" ];
    };

  boot.initrd.luks.devices."cryptnvme".device = "/dev/disk/by-uuid/f1dc12d5-9a75-4e28-a747-a098333614ac";
  boot.initrd.luks.devices."cryptnvme1".device = "/dev/disk/by-uuid/2f7640e7-532f-4404-90fd-b3c065530d42";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/9C5A-6D6F";
      fsType = "vfat";
    };

  fileSystems."/.backup" =
    { device = "/dev/disk/by-uuid/0bd10808-0330-4736-9425-059d4a0a300e";
      fsType = "btrfs";
      options = [ "compress=lzo" ];
    };

  boot.initrd.luks.devices."cryptsda1".device = "/dev/disk/by-uuid/5592422a-b0f9-4569-af33-2f47bf2d8079";
  boot.initrd.luks.devices."cryptsdb1".device = "/dev/disk/by-uuid/49e28c9b-506e-4f56-b9ef-3e22c6e06683";

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 32;

  networking = {
    hostName = "ryzen3950";
    wireless = {
      enable = true;
      networks = import ./security/wifi.nix;
    };

    useDHCP = true;
    # useDHCP = false;
    # interfaces = {
    #   enp4s0.useDHCP = true;
    #   enp5s0.useDHCP = true;
    #   virbr0.useDHCP = true;
    #   virbr0-nic.useDHCP = true;
    #   wlp6s0.useDHCP = true;
    # };
    dhcpcd.persistent = true;
  };

  environment.systemPackages = with pkgs; [
    radeontop
    krakenx
  ];

  environment.variables.XCURSOR_SIZE = "32";

  services = {
    xserver = {
      videoDrivers = [ "amdgpu" ];
      resolutions = [ { x = 3840; y = 2160; } ];
      dpi = 192;
      defaultDepth = 24;
    };
  };
}
