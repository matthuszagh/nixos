{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    ntfs3g
    ddrescue
    smartmontools
  ];
}
