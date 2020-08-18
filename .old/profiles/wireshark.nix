{ config, pkgs, ... }:

{
  programs.wireshark.enable = true;
  environment.systemPackages = with pkgs; [ wireshark ];
  users.users.matt.extraGroups = [ "wireshark" ];
}
