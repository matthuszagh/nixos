{ stdenv, fetchFromGitHub, emacs, pkgs }:

let
  emacs-with-packages = (pkgs.emacsPackagesGen emacs).emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    helm
    helm-org
    dash
    dash-functional
    f
    ts
    org-super-agenda
    peg
    ov
  ]) ++ (with epkgs.orgPackages; [
    org
  ]));
in
stdenv.mkDerivation rec {
  name = "org-ql";

  src = /home/matt/src/org-ql;

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
