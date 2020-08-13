{ ... }:

{
  programs.keychain = {
    enable = true;
    agents = [ "gpg" "ssh" ];
    extraFlags = [ "--quiet" "--nogui" ];
    keys = [ "id_rsa" "086A045AACCFA2659A356ED3A5F2E8C3791245EA" ];
    enableBashIntegration = true;
  };
}
