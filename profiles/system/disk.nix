{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    ntfs3g
    ddrescue
    smartmontools
    iotop
    disk-utilities
  ];
}
