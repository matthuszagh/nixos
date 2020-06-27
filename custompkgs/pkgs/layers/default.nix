{ stdenv, fetchFromGitHub, emacs, pkgs }:

let
  emacs-with-packages = (pkgs.emacsPackagesGen emacs).emacsWithPackages (epkgs:(with epkgs.melpaPackages; [
    ht
    dash
  ]));
in
stdenv.mkDerivation rec {
  name = "layers";

  src = /home/matt/src/layers;

  propagatedBuildInputs = [
    emacs-with-packages
  ];

  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    install *.el* $out/share/emacs/site-lisp
  '';

  meta = {
    description = "Dependency management for package collections.";
    homepage = https://github.com/matthuszagh/layers;
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.all;
  };
}
