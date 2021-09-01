{ pkgs
, ...
}:

{
  console = {
    # Packages made available to the console. Console fonts should be
    # specified here.
    packages = with pkgs; [
      terminus_font
    ];

    # We don't explicitly set this to give systemd-vconsole
    # flexibility to choose a suitable font.
    # font = "ter-v32n";

    # set fonts in initramfs
    earlySetup = true;
    # switch caps-lock and ctrl
    useXkbConfig = true;
  };
}
