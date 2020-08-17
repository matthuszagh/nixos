{ pkgs, ... }:

{
  home.file.".notmuch-config".source = ./config;
}
