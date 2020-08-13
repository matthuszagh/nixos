{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    less
    vim
  ];

  console = {
    # set fonts in initramfs
    earlySetup = true;
    # switch caps-lock and ctrl
    useXkbConfig = true;
    # make the console font legible on the HiDPI display.
    font = "latarcyrheb-sun32";
  };

  programs.thefuck.enable = true;
}
