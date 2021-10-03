# TODO this should be part of the flake!
{ config
, pkgs
, ...
}:

{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  # programs that should be available in the installer
  environment.systemPackages = with pkgs; [
    fish
    git
  ];

  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
}
