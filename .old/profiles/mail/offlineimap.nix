{ pkgs
, ...
}:

{
  services.offlineimap = {
    enable = true;
    install = true;
    path = with pkgs; [
      bash
      notmuch
    ];
    timeoutStartSec = "1h";
  };
}
