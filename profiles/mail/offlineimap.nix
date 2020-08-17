{ pkgs
, user
, ...
}:

{
  systemd.user.services.offlineimap = {
    description = "Offlineimap: a software to dispose your mailbox(es) as a local Maildir(s)";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "offlineimap -u syslog -o -1";
      User = user;
      TimeoutStartSec = "3600sec";
    };
    path = with pkgs; [
      offlineimap
      notmuch
    ];
  };
}
