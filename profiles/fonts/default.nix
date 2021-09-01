{ pkgs
, ...
}:

{
  fonts = {
    # Provides a collection of commonly-used font libraries. This
    # gives reasonable unicode coverage.
    enableDefaultFonts = true;

    fontconfig = {
      # tells some graphical applications where to find default fonts
      enable = true;

      # default system fonts
      defaultFonts = {
        emoji = [ "Noto Color Emoji" ];
        monospace = [ "Source Code Pro Regular" ];
        sansSerif = [ "DejaVu Sans" ];
        serif = [ "DejaVu Serif" ];
      };
    };


    fonts = with pkgs; [
      source-code-pro
    ];
  };
}
