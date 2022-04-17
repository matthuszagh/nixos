{ pkgs
, ...
}:

{
  systemd.services.recollindex = {
    description = "Periodically run recollindex";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = ''
        ${pkgs.recoll}/bin/recollindex
      '';
      Restart = "always";
      # disable timeout
      TimeoutSec = "infinity";
      User = "matt";
    };
    path = with pkgs; [
      recoll
      aspell
      exiftool # TODO needed?
    ];
  };
}
