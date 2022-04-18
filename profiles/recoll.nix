{ pkgs
, ...
}:

{
  systemd.services.recollindex = {
    description = "Run recollindex";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.recoll}/bin/recollindex
      '';
      User = "matt";
    };
    path = with pkgs; [
      recoll
      aspell
      exiftool # TODO needed?
    ];
  };

  systemd.timers.recollindex = {
    description = "Run recollindex.service daily.";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      Unit = "recollindex.service";
      OnCalendar = "daily";
    };
  };
}
