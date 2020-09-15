{ pkgs
, ...
}:

{
  environment.etc."btrbk.conf".source = ./btrbk.conf;

  systemd.services.btrfs-backup = {
    description = "Backup home directory.";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.btrbk}/bin/btrbk -q run
      '';
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
