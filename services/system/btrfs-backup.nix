{ pkgs, ... }:

{
  systemd.services.btrfs-backup = {
    description = "Backup home directory.";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = /home/matt/src/dotfiles/services/system/btrfs-backup.sh;
      User = "root";
    };
    path = with pkgs; [
      btrbk
    ];
  };

  systemd.timers.btrfs-backup = {
    description = "Run btrfs-backup.sh hourly.";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      Unit = "btrfs-backup.service";
      OnCalendar = "hourly";
    };
  };
}
