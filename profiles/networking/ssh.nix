{ pkgs
, ...
}:

{
  programs.ssh = {
    # openssh_hpn is a fork of openssh that can provide drastically
    # higher performance.
    package = pkgs.openssh_hpn;
    # Ciphers are given in order of preference, so the second will
    # only be used if the first is unavailable. These ciphers should
    # provide significantly faster performance than the defaults.
    ciphers = [ "aes128-gcm@openssh.com" "aes256-gcm@openssh.com" ];
  };

  services.openssh = {
    enable = true;
    forwardX11 = true;
    # Disables password authentication and requires that the
    # connecting host's public key be available in known_hosts.
    passwordAuthentication = false;
    permitRootLogin = "no";
    allowSFTP = true;
  };

  # The SSH agent remembers private keys so that you don't have to
  # type in a passphrase every time you want to make an SSH
  # connection.
  programs.ssh.startAgent = true;
}
