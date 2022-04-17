{ pkgs
, ...
}:

{
  systemd.services.mbsync = {
    description = "Synchronize email with servers and index with notmuch.";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = ''
        ${pkgs.isync}/bin/mbsync -a
      '';
      ExecStartPost = ''
        ${pkgs.notmuch}/bin/notmuch new
      '';
      Restart = "always";
      # disable timeout
      TimeoutSec = "infinity";
      User = "matt";
    };
    path = with pkgs; [
      isync
      notmuch
      gawk
    ];
  };
}
