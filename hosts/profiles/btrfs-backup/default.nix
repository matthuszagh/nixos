{ pkgs
, ...
}:

{
  environment.etc."btrbk.conf".source = ./btrbk.conf;

  # environment.systemPackages = with pkgs; [
  #   borgbackup
  # ];

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

  # systemd.services.borg-backup = {
  #   description = "Backup home directory to borgbase.";
  #   wantedBy = [ "multi-user.target" ];
  #   serviceConfig = {
  #     Type = "oneshot";
  #     ExecStart = ''
  #       ./borg-backup.sh
  #     '';
  #     User = "root";
  #   };
  #   path = with pkgs; [
  #     borgbackup
  #   ];
  # };

  # systemd.timers.btrfs-backup = {
  #   description = "Run btrfs-backup.sh daily.";
  #   wantedBy = [ "timers.target" ];
  #   timerConfig = {
  #     Unit = "borg-backup.service";
  #     OnCalendar = "daily";
  #   };
  # };
}
