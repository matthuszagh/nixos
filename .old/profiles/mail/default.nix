{ pkgs
, ...
}:

{
  imports = [
    ./offlineimap.nix
  ];

  environment.systemPackages = with pkgs; [
    notmuch
  ];
}
