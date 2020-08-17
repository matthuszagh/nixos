{ ... }:

{
  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 60480000;
    maxCacheTtl = 60480000;
    # extraConfig = ''
    #   allow-emacs-pinentry
    # '';
    sshKeys = [ "id_rsa" ];
  };
}
