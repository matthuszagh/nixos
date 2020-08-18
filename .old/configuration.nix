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

  # import paths
  src-path = "/home/matt/src";
  config-path = "/home/matt/src/dotfiles/nixos/config";
  modules-path = "/home/matt/src/dotfiles/nixos/modules";
  services-path = "/home/matt/src/dotfiles/nixos/services";
  nixos-path = "/home/matt/src/dotfiles/nixos";
  emacs-path = "/home/matt/src/dotfiles/emacs";
in
{
  # system-wide packages
  environment.systemPackages = with pkgs; [
    # benchmarking
    scc

    # python3Packages.hdl_checker
    # must be root available for proper permissions
    nix-generate-from-cpan
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
  ];
  # ] ++ builtins.filter stdenv.lib.isDerivation (builtins.attrValues kdeApplications);

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
  };

  hardware = {
    # enable access to scanners
    sane = {
      enable = true;
      snapshot = true;
    };
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
  };

  system.stateVersion = "19.03";
}
