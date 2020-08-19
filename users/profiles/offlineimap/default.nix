{ pkgs
, ...
}:

{
  xdg.configFile."offlineimap/config".source = ./config;
  xdg.configFile."offlineimap/postsync.sh".source = ./postsync.sh;
}
