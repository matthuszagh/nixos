{ pkgs
, ...
}:

{
  imports = [
    ./c
    ./console
    ./emacs
    ./git.nix
    ./jupyter
    ./lorri.nix
    ./nix
    ./python
    ./rust
    ./shell
    ./tex
    ./typescript
    ./verilog
    ./wireshark.nix
  ];

  environment.systemPackages = with pkgs; [
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
    binutils
    usbutils
    wget
    curl
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

    # ====================== reverse engineering =====================
    ghidra-bin
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
