{ config, lib, pkgs, ... }:
let
  custompkgs = import <custompkgs> { };
  nur = import <nur> { inherit pkgs; };
  home-manager = (builtins.fetchTarball {
    name = "home-manager-unstable-2020-07-20";
    url = "https://github.com/rycee/home-manager/archive/8ad55800216760291e253150a7ecc831c2956229.tar.gz";
    sha256 = "0nif0a2pv5wgyjhfqd8jmlc055y0604xmqfnnr2hfrzz0blpl4ww";
  });
  sagemath-pkgs = (import (builtins.fetchTarball {
    name = "nixpkgs-unstable-2020-05-01";
    url = "https://github.com/nixos/nixpkgs/archive/7866440f1223dc447f38cb23c00e10b44b4c98fe.tar.gz";
    sha256 = "1fh5yglh8l1r1flvfayk61vpsmq2q21g5pqmjjqqhp2bz7c3psbr";
  }) {});

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
  src-path = "/home/matt/src";
  config-path = "/home/matt/src/dotfiles/nixos/config";
  modules-path = "/home/matt/src/dotfiles/nixos/modules";
  services-path = "/home/matt/src/dotfiles/nixos/services";
  nixos-path = "/home/matt/src/dotfiles/nixos";
  emacs-path = "/home/matt/src/dotfiles/emacs";
in
{
  imports = [
    /etc/nixos/hardware-configuration.nix

    # add home-manager, which is used to manager user configurations
    "${home-manager}/nixos"

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
    (src-path + "/dotfiles/nixos/custompkgs/pkgs/pia")
    (modules-path + "/fish.nix")
    (modules-path + "/bash.nix")
    (modules-path + "/emacs/")
    (modules-path + "/bash.nix")
    (modules-path + "/offlineimap")
    # TODO fix
    # (modules-path + "/nix-tree.nix")
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
      "custompkgs=${nixos-path}/custompkgs" # private pkgs repo
      "nur=${src-path}/NUR" # Nix User Repositories
      "nixpkgs=${src-path}/nixpkgs" # use local mirror of nixpkgs collection
      "nixpkgs-overlays=${nixos-path}/overlays"
      "nixos-config=${nixos-path}/configuration.nix"
    ];

    # keep-outputs preserves source files and other non-requisit parts
    # of the build process.  keep-derivations preserves derivation
    # files, which can be useful to query build dependencies, etc.
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';

    binaryCaches = [
      "https://cache.nixos.org"
      # "http://ryzen3950:80"
    ];
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
    sagemath-pkgs.sageWithDoc
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
        (builtins.readFile ./security/postgresql);
    };
  };

  nixpkgs.overlays = (
    let path = ./overlays;
    in with builtins; map (n: import (path + ("/" + n)))
    (filter (n: match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix")))
    (attrNames (readDir path)))
  );

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
      (config-path + "/notmuch.nix")
      (config-path + "/clang-format.nix")
      (config-path + "/recoll.nix")
    ];
  };

  system.stateVersion = "19.03";
}
