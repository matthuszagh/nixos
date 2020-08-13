{ stdenv, fetchFromGitHub, emacs, pkgs }:

let
  emacs-with-packages = (pkgs.emacsPackagesGen emacs).emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    helm
  ]));
in
stdenv.mkDerivation rec {
  name = "helm-ls-git";

  src = fetchFromGitHub {
    owner = "emacs-helm";
    repo = "helm-ls-git";
    rev = "master";
    sha256 = "0r4dj6422g2zsbm2kig369j39lw4d68zpcr2xr02p02sfyhg1c0f";
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
