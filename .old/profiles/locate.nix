{ config, pkgs, ... }:

{
    services.locate = {
      enable = true;
      interval = "hourly";
      prunePaths = [
        "/tmp"
        "/var/tmp"
        "/var/cache"
        "/var/lock"
        "/var/run"
        "/var/spool"
        "/nix/store"
        "/.snapshots"
        "/.backup"
        "/home/matt/.local"
      ];
    };
}
