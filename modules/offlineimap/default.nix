{ config, pkgs, ... }:

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

  home-manager.users.matt = { config, ... }: rec {
    xdg.configFile."offlineimap/config".text = ''
      [general]
      accounts = gmail
      maxsyncaccounts = 1

      [Account gmail]
      localrepository = gmail-Local
      remoterepository = gmail-Remote
      synclabels = yes
      autorefresh = 1

      postsynchook = ${config.home.homeDirectory}/.config/offlineimap/postsync.sh

      [Repository gmail-Local]
      type = GmailMaildir
      localfolders = /home/matt/mail/gmail

      [Repository gmail-Remote]
      type = Gmail
      remoteuser = huszaghmatt@gmail.com
      remotepass = sprzmhjdzgfbkffv

      sslcacertfile = /etc/ssl/certs/ca-certificates.crt

      folderfilter = lambda foldername: foldername in ['[Gmail]/All Mail']
      nametrans = lambda foldername: re.sub('^\[Gmail\]/All Mail$', 'all',foldername)
      holdconnectionopen = yes
    '';
    # xdg.configFile."offlineimap/config".source = ./config;
    xdg.configFile."offlineimap/postsync.sh".source = ./postsync.sh;
  };
}
