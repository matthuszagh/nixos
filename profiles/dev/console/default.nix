{ pkgs
, ...
}:

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
  };

  programs.thefuck.enable = true;
}
