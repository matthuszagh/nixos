{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    notmuch
    isync
    mb2md # convert mbox to maildir
  ];
}
