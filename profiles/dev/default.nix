{ pkgs
, ...
}:

{
  imports = [
    ./bash
    ./c
    ./console
    ./emacs
    ./nix
    ./python
    ./rust
    ./tex
    ./typescript
    ./verilog
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
  ];

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  };
}
