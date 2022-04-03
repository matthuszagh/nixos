{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    notmuch
    isync
  ];
}
