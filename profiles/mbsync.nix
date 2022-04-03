{ pkgs
, ...
}:

{
  systemd.services.mbsync = {
    description = "Synchronize email with servers and index with notmuch.";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.isync}/bin/mbsync -a
      '';
      ExecStartPost = ''
        ${pkgs.notmuch}/bin/notmuch new
      '';
      User = "matt";
    };
    path = with pkgs; [
      isync
      notmuch
      gawk
    ];
  };

  systemd.timers.mbsync = {
    description = "Run mbsync.service periodically.";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      Unit = "mbsync.service";
      # run every 5 minutes
      OnCalendar = "*-*-* *:00/5:00";
    };
  };
}
