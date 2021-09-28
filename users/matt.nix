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
      "cdrom" # cd drive
    ];
  };

  # resource limits
  security.pam.loginLimits = [
    {
      domain = "matt";
      type = "soft";
      item = "nofile";
      value = "524288";
    }
  ];

  home-manager.users.matt = { pkgs, ... }: {
    imports = [
      ./profiles/xorg
      ./profiles/git
      ./profiles/git/matt.nix
      ./profiles/gpg
      ./profiles/keychain
      ./profiles/chktex
      ./profiles/direnv
      ./profiles/clang-format
      ./profiles/inkscape
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
      ./profiles/next
    ];

    # TODO fix (see https://github.com/nix-community/home-manager/issues/1479)
    manual.manpages.enable = false;
  };
}
