{ pkgs
, ...
}:

{
  imports = [
    ./emacs
    ./c
    ./python
    ./console
  ];

  environment.systemPackages = with pkgs; [
    glibcInfo
    clang-manpages
    llvm-manpages
    stdman # cppreference manpages
    stdmanpages
    man-pages # linux manpages
    posix_man_pages
  ];

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  }
}
