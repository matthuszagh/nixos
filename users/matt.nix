{
  users.users.matt = {
    isNormalUser = true;
    password = builtins.readFile ../secrets/matt-password;
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
}
