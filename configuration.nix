{ config, lib, pkgs, ... }:
let
  custompkgs = import <custompkgs> { };
  nur = import <nur> { inherit pkgs; };

  python-with-packages = pkgs.python3Full.withPackages (p: with p; [
    # TODO fix
    # custompkgs.skidl
    # custompkgs.libcircuit
    sympy
    ipython
    numpy
    mypy
    # ptvsd
    # pylibgen
  ] ++ (with custompkgs; [
    # pylibgen
  ]));

  # import paths
  config-path = /etc/nixos/config;
  modules-path = /etc/nixos/modules;
  services-path = /etc/nixos/services;
  src-path = /home/matt/src;
in
{
  imports = [
    ./hardware-configuration.nix

    # add home-manager, which is used to manager user configurations
    (src-path + "/home-manager/nixos")

    # disable linux security features to increase performance
    #../config/make-linux-fast-again.nix
    # TODO get working
    # enable numlock always
    # ../../config/services/numlock.nix

    # ============================ system ============================
    (modules-path + "/udev.nix")
    (modules-path + "/locate.nix")
    (modules-path + "/sound.nix")
    (modules-path + "/power.nix")
    (modules-path + "/ssh.nix")
    (modules-path + "/gnupg.nix")
    (modules-path + "/lorri.nix")
    (modules-path + "/syncthing.nix")
    (modules-path + "/wireshark.nix")

    # =========================== userspace ==========================
    (services-path + "/user/offlineimap.nix")
    (src-path + "/dotfiles/custompkgs/pkgs/pia")
    (modules-path + "/fish.nix")
    (modules-path + "/bash.nix")
    (modules-path + "/emacs.nix")
    (modules-path + "/bash.nix")
  ];

  # options configuring nix's behavior
  nix = {
    useSandbox = true;
    nrBuildUsers = 100;

    package = pkgs.nixFlakes;
    # use a local repo for nix to test out experimental changes
    # package = pkgs.nixUnstable.overrideAttrs (old: {
    #   src = (src-path + "/nix");
    #   # buildInputs = old.buildInputs ++ (with pkgs; [
    #   #   bison
    #   #   flex
    #   # ]);
    # });
    nixPath = [
      "custompkgs=/home/matt/src/dotfiles/custompkgs" # private pkgs repo
      "nur=/home/matt/src/NUR" # Nix User Repositories
      "nixpkgs=/home/matt/src/nixpkgs" # use local mirror of nixpkgs collection
      "nixpkgs-overlays=/etc/nixos/overlays"
      "nixos-config=/etc/nixos/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];

    # keep-outputs preserves source files and other non-requisit parts
    # of the build process.  keep-derivations preserves derivation
    # files, which can be useful to query build dependencies, etc.
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
  };

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        consoleMode = "max";
        editor = true;
      };

      efi.canTouchEfiVariables = true;
    };

    cleanTmpDir = true;

    # use the latest stable linux kernel.
    kernelPackages = pkgs.linuxPackages_latest;
  };

  console = {
    # set fonts in initramfs
    earlySetup = true;
    # use the same caps-lock / ctrl switch
    useXkbConfig = true;
    # make the console font legible on the HiDPI display.
    font = "latarcyrheb-sun32";
  };

  system.autoUpgrade.enable = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Los_Angeles";

  # system-wide packages
  environment.systemPackages = with pkgs; [
    # core
    coreutils
    binutils
    usbutils
    git
    git-lfs
    wget
    curl
    zip
    unzip
    acpi # TODO separate from pciutils?
    pciutils
    tlp
    wpa_supplicant
    powertop
    pinentry
    # disk partition
    parted
    # displays hardware information
    dmidecode
    # benchmarking
    phoronix-test-suite
    sloccount
    scc

    # editing
    vim

    # dev
    gnumake
    # TODO should be bundled with Emacs
    glibcInfo
    clang-manpages
    llvm-manpages
    stdman # cppreference manpages
    stdmanpages
    man-pages # linux manpages
    posix_man_pages
    openocd
    libftdi1
    gdb
    # TODO fix
    sageWithDoc
    # python3Packages.hdl_checker
    # must be root available for proper permissions
    nix-generate-from-cpan
    perlPackages.LaTeXML
    compsize

    # keyboard
    numlockx
    # audio
    pavucontrol
    sddm
    obs-studio

    # utilities
    # move deleted files to trash rather than permanently deleting them.
    trash-cli
    # bridge between network interface and software
    bridge-utils
    # pdf editor
    k2pdfopt
    pdftk
    google-chrome

    # canon printer
    canon-cups-ufr2

    # mail
    notmuch

    # graphics
    # TODO should this be available to root?
    mesa
    xlibs.xwininfo
    xlibs.xhost
    xlibs.xdpyinfo
    glxinfo
    gnome3.gnome-settings-daemon
    breeze-icons
  ];
  # ] ++ builtins.filter stdenv.lib.isDerivation (builtins.attrValues kdeApplications);

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
    # nixos.includeAllModules = true;
  };

  services = {
    gnome3.gnome-settings-daemon.enable = true;
    # compositing manager, replacement for built-in EXWM compositor
    # which apparently has issues.
    # compton = {
    #   enable = true;
    #   vSync = true;
    #   backend = "glx";
    # };

    # needed for next-browser
    # dbus.enable = true;

    # spice support for virtual machines.
    # spice-vdagentd.enable = true;

    # PostgreSQL server
    postgresql = {
      enable = true;
      package = pkgs.postgresql_10;
      enableTCPIP = true;
      authentication = pkgs.lib.mkOverride 10 ''
        local all all trust
        host all all ::1/128 trust
      '';
      initialScript = pkgs.writeText
        "backend-initScript"
        builtins.readFile ./security/postgresql;
    };
  };

  # TODO move emacs overlay to emacs module
  nixpkgs.overlays =
    (
      let path = ./overlays;
      in
      with builtins;
      map
        (n: import (path + ("/" + n)))
        (
          filter
            (n: match ".*\\.nix" n != null ||
            pathExists (path + ("/" + n + "/default.nix")))
            (attrNames (readDir path))
        )) ++ [
      (import (builtins.fetchTarball {
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
      }))
    ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  virtualisation = {
    libvirtd.enable = true;
  };

  hardware = {
    # TODO opengl should probably get its own module. There is opengl
    # config elsewhere too, such as oryp4.
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    # enable access to scanners
    sane = {
      enable = true;
      snapshot = true;
    };
  };

  fonts = {
    fonts = with pkgs; [
      source-code-pro
    ];
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups = { plugdev = { }; };

  users.users.matt = {
    isNormalUser = true;
    description = "Matt Huszagh";
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
    # programs = {
    #   firefox.enable = true;
    # };

    services.pasystray.enable = true;

    xdg.enable = true;

    nixpkgs.config = import (config-path + "/nixpkgs-config.nix");
    xdg.configFile."nixpkgs/config.nix".source = (config-path + "/nixpkgs-config.nix");

    # user packages that do not require/support home-manager
    # customization (they may still have overlays)
    home.packages = with pkgs; [
      ## browsers
      w3m
      speedtest-cli
      glib-networking

      ## programming
      dos2unix
      (lib.hiPrio gcc)
      custompkgs.clang_multi_9
      # clang_9
      # llvm_9
      gfortran
      cmake
      cask
      # TODO bundle with emacs
      python-with-packages
      # TODO fix
      # gscan2pdf # connect to scanners

      # TODO fix
      # hackrf
      # rtl-sdr
      # gnuradio

      ## utilities
      tree
      unrar
      vdpauinfo
      nox
      nix-review
      ghostscript
      # utility for DJVU. allows converting djvu to pdf with ddjvu
      djvulibre
      firefox

      ## math
      octave
      paraview
      asymptote

      ## media
      dolphinEmu
      transmission
      transgui
      mpv
      kdenlive

      ## OS emulation
      # TODO fix
      # wine

      ## 3D printing
      # TODO fix
      # cura

      # Private nixpkgs repo. I use this as a staging area for pkgs
      # not yet ready for the main nixpkgs repo and for packages that
      # will never be fit for nixpkgs.
    ] ++ (with custompkgs; [
      # kicad
    ]);

    imports = [
      (config-path + "/emacs.nix")
      (config-path + "/git.nix")
      (config-path + "/keychain.nix")
      (config-path + "/gpg.nix")
      (config-path + "/ngspice.nix")
      (config-path + "/direnv.nix")
      (config-path + "/pylint.nix")
      # (config-path + "/next.nix")
      # TODO this interferes with kicad-written files
      # ../config/kicad.nix
      (config-path + "/tex.nix")
      (config-path + "/chktex.nix")
      (config-path + "/octave.nix")
      (config-path + "/sage.nix")
      (config-path + "/offlineimap.nix")
      (config-path + "/notmuch.nix")
      (config-path + "/clang-format.nix")
      (config-path + "/recoll.nix")
    ];
  };

  system.stateVersion = "19.03";
}
