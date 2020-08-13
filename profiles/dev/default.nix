{ pkgs, ... }:

{
  imports = [
    ./emacs
    ./c
    ./python
  ];

  documentation.dev.enable = true;
}
