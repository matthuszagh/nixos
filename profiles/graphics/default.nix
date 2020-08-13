{ pkgs, ... }:

{
  fonts = {
    fonts = with pkgs; [
      source-code-pro
    ];
  };
}
