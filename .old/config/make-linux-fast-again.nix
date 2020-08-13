{ pkgs, config, ... }:

let
  cmdline = builtins.readFile (
    /home/matt/src/make-linux-fast-again
    # builtins.fetchurl "https://make-linux-fast-again.com"
  );
in {
  boot.kernelParams = pkgs.lib.splitString " " cmdline;
}
