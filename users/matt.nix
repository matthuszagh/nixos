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

  home-manager.users.matt = { pkgs, ... }: {
    imports = [
      ./profiles/emacs
      ./profiles/xorg
      ./profiles/git
      ./profiles/git/matt.nix
      ./profiles/gpg
      ./profiles/keychain
      ./profiles/chktex
      ./profiles/direnv
      ./profiles/clang-format
      ./profiles/ngspice
      ./profiles/octave
      ./profiles/notmuch
      ./profiles/offlineimap
      ./profiles/pylint
      ./profiles/recoll
      ./profiles/sage
      ./profiles/tex
      ./profiles/fish
      ./profiles/bash
    ];
  };
}
