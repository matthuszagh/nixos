{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    ntfs3g
    ddrescue
    smartmontools
    iotop
    libdsk # disk image utilities
    disk-utilities
    lifutils
  ];
}
