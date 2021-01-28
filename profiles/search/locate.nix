{ pkgs
, ...
}:

{
  services.locate = {
    enable = true;
    interval = "hourly";
    localuser = "root";
    locate = pkgs.findutils;
    prunePaths = [
      "/tmp"
      "/var/tmp"
      "/var/cache"
      "/var/lock"
      "/var/run"
      "/var/spool"
      "/nix"
      "/.snapshots"
      "/.backup"
      "/home/matt/.local"
      "/home/matt/.cache"
    ];
  };
}
