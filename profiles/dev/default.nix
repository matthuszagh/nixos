{ pkgs, ... }:

{
  imports = [
    ./emacs
    ./c
    ./python
    ./console
  ];

  documentation.dev.enable = true;
}
