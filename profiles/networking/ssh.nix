{ ... }:

{
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
