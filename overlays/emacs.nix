self: super:

let
  version = "28";
  versionModifier = ".0.50";
  pkgs = (import (builtins.fetchTarball {
    name = "nixos-staging-2020-05-16";
    url = "https://github.com/nixos/nixpkgs/archive/75c55c4fe2a3defd87cfee7e6f997e6445207cb9.tar.gz";
    sha256 = "06pxm15pnbm4n63rdjb5wfkqxv32m8pd2wwdr9hqqyydhmcd4wrc";
  }) {});
in
{
  emacs = (super.emacs.override {
    stdenv = pkgs.gcc9Stdenv;
    withGTK3 = true;
    withXwidgets = true;
    withX = true; # provides imagemagick and other GUI-related support.
    srcRepo = true;
    withCsrc = true;
  }).overrideAttrs (attrs: {
    name = "emacs-${version}${versionModifier}";
    version = version;
    versionModifier = versionModifier;
    src = /home/matt/src/emacs;
    CFLAGS = "-O3 -march=native -momit-leaf-frame-pointer";
    buildInputs = attrs.buildInputs ++ (with pkgs; [
      jansson
      libgccjit
    ]);
    patches = [];
    configureFlags = attrs.configureFlags ++ [ "--with-nativecomp" ];
    LIBRARY_PATH = "${pkgs.lib.getLib pkgs.stdenv.cc.libc}/lib";
  });
}
