{ config
, lib
, pkgs
, ...
}:

{
  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot = {
        enable = true;
        consoleMode = "max";
        editor = true;
      };

      efi.canTouchEfiVariables = true;
    };

    # delete /tmp contents at each boot
    cleanTmpDir = true;

    # use the latest stable linux kernel.
    kernelPackages = pkgs.linuxPackages_latest;
  };
}
