{ pkgs
, ...
}:

{
  imports = [
    ./c
    ./emacs
    ./git.nix
    ./jupyter
    ./lorri.nix
    ./nix
    ./python
    ./rust
    ./reverse-engineering
    ./shell
    ./tex
    ./typescript
    ./verilog
    ./wireshark.nix
  ];

  environment.systemPackages = with pkgs; [
    # TODO place this in their own directories

    # ========================= documentation ========================
    glibcInfo
    clang-manpages
    llvm-manpages
    stdman # cppreference manpages
    stdmanpages
    man-pages # linux manpages
    posix_man_pages

    # ============================ search ============================
    ripgrep

    # ============================= core =============================
    coreutils
    moreutils
    # binutils
    (binutils-unwrapped.override {
      withAllTargets = true;
      # needed to get this working
      # see https://github.com/NixOS/nixpkgs/issues/82792
      enableShared = false;
    })
    usbutils
    acpi
    pciutils
    gnumake
    parted
    dmidecode # displays hardware information
    phoronix-test-suite # benchmarking hardware
    dos2unix
    clang_multi_9
    gcc
    openssl
    pkg-config
    llvm_11
    file # display file type/information

    # ============================ archive ===========================
    unrar
    zip
    unzip
    p7zip

    # ============================= misc =============================
    scc
    tree

    # =================== performance and debugging ==================
    linuxPackages.perf
    gperftools
    valgrind

    # =========================== embedded ===========================
    openocd
    gdb
  ];

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
    nixos.enable = true;
  };
}
