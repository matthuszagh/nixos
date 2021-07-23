{ lib
, pkgs
, ...
}:

{
  imports = [
    ./profiles/systemd-boot.nix
    ./profiles/btrfs-backup
    ./profiles/btrfs.nix
  ];

  boot = {
    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ ];
      luks = {
        devices = {
          "cryptnvme".device = "/dev/disk/by-uuid/f1dc12d5-9a75-4e28-a747-a098333614ac";
          "cryptnvme1".device = "/dev/disk/by-uuid/2f7640e7-532f-4404-90fd-b3c065530d42";
          # "cryptsda1".device = "/dev/disk/by-uuid/5592422a-b0f9-4569-af33-2f47bf2d8079";
          "cryptsdb1".device = "/dev/disk/by-uuid/49e28c9b-506e-4f56-b9ef-3e22c6e06683";
          "cryptsdc1".device = "/dev/disk/by-uuid/74533b18-c153-4883-bf24-0e033a1f9547";
          "cryptsdd1".device = "/dev/disk/by-uuid/1216a974-ae83-406f-a684-1d776d7d57b0";
        };
      };
    };

    kernelModules = [ "kvm-amd" ];
    kernelParams = [ "fbcon=rotate:3" ];
    extraModulePackages = [ ];
  };

  hardware.cpu.amd.updateMicrocode = true;
  hardware.video.hidpi.enable = lib.mkDefault true;

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/d31878d6-3a77-4f0f-9fdd-bb9a2c4e578b";
      fsType = "btrfs";
      options = [ "subvol=nixos" "compress=zstd:2" "ssd" "noatime" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/9C5A-6D6F";
      fsType = "vfat";
    };

  fileSystems."/.backup" =
    {
      device = "/dev/disk/by-uuid/0bd10808-0330-4736-9425-059d4a0a300e";
      fsType = "btrfs";
      options = [ "compress=zstd:2" ];
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 32;

  networking = {
    wireless = {
      enable = false;
      networks = import ../secrets/wifi.nix;
    };

    useDHCP = true;
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
      resolutions = [{ x = 3840; y = 2160; }];
      dpi = 192;
      defaultDepth = 24;
    };
  };
}
