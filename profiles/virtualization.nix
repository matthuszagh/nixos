{ pkgs
, ...
}:

{
  virtualisation = {
    libvirtd = {
      enable = true;
      qemuPackage = (pkgs.qemu.override { smbdSupport = true; }); # qemu with samba support
    };
  };
}
