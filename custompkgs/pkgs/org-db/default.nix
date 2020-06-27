{ stdenv, fetchFromGitHub, emacs, pkgs }:

let
  emacs-with-packages = (pkgs.emacsPackagesGen emacs).emacsWithPackages (epkgs:(with epkgs.melpaPackages; [
    emacsql
    emacsql-sqlite
    s
    helm
  ]));
in
stdenv.mkDerivation rec {
  name = "org-db";

  src = /home/matt/src/org-db;

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
    description = "Index org files.";
    # homepage = https://github.com/matthuszagh/layers;
    # license = stdenv.lib.licenses.gpl3;
    # platforms = stdenv.lib.platforms.all;
  };
}
