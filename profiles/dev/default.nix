{ pkgs
, ...
}:

{
  imports = [
    ./c
    ./console
    ./emacs
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
    stdman  # cppreference manpages
    stdmanpages
    man-pages  # linux manpages
    posix_man_pages

    # ============================ search ============================
    ripgrep

    # ============================= core =============================
    coreutils
    binutils
    usbutils
    wget
    curl
    zip
    unzip
    acpi
    pciutils
    gnumake
    parted
    dmidecode  # displays hardware information
    phoronix-test-suite  # benchmarking hardware
    dos2unix
    clang_multi_9
    gcc

    # ============================= misc =============================
    scc
    tree
    unrar
  ];

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  };
}
