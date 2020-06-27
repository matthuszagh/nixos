{ stdenv, fetchFromGitHub, emacs, pkgs }:

let
  emacs-with-packages = (pkgs.emacsPackagesGen emacs).emacsWithPackages (epkgs:(with epkgs.melpaPackages; [
    async
  ]));
in
stdenv.mkDerivation rec {
  name = "helm";

  src = fetchFromGitHub {
    owner = "emacs-helm";
    repo = "helm";
    rev = "master";
    sha256 = "0b3qb4pclk7xc0j2dsbfakp8s04vz484zcd0s0yzmsfdvz8nc2y8";
  };

  buildInputs = [
    emacs-with-packages
  ];

  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    install *.el* $out/share/emacs/site-lisp
  '';
}
