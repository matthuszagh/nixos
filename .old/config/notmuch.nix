{ pkgs, ... }:

{
  home.file.".notmuch-config".source = ./notmuch/config;
}
