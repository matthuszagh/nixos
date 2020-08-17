{
  imports = [ ];

  users.users.matt = {
    isNormalUser = true;
    passwordFile = ../../secrets/matt-password;
    description = "Matt Huszagh";
    # TODO document why each of these are needed
    extraGroups = [
      "wheel"
      "video"
      "audio"
      "disk"
      "networkmanager"
      "plugdev"
      "dialout"
      "libvirtd"
      "scanner" # scanners
      "lp" # printers
    ];
  };

  home-manager.users.matt = {
    imports = [
      ../profiles/emacs
    ];
  };
}
