{ pkgs, ... }:

{
  systemd.user.services.offlineimap = {
    description = "Offlineimap: a software to dispose your mailbox(es) as a local Maildir(s)";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "offlineimap -u syslog -o -1";
      User = "matt";
      TimeoutStartSec = "3600sec";
    };
    path = with pkgs; [
      offlineimap
      notmuch
    ];
  };

  # systemd.user.timers.offlineimap = {
  #   description = "Run offlineimap periodically.";
  #   wantedBy = [ "timers.target" ];
  #   timerConfig = {
  #     Unit = "offlineimap.service";
  #     OnCalendar = "*:0/3";
  #     Persistent = "true";
  #   };
  # };
}
