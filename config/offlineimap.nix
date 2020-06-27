{ pkgs, ... }:

{
  xdg.configFile."offlineimap/config".source = ./offlineimap/config;
  xdg.configFile."offlineimap/postsync.sh".source = /home/matt/src/tools/offlineimap/postsync.sh;
}
