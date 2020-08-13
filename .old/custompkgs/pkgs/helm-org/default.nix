{ stdenv, fetchFromGitHub, emacs, pkgs }:

let
  emacs-with-packages = (pkgs.emacsPackagesGen emacs).emacsWithPackages (epkgs:(with epkgs.melpaPackages; [
    helm
  ]));
in
stdenv.mkDerivation rec {
  name = "helm-org";

  src = fetchFromGitHub {
    owner = "emacs-helm";
    repo = "helm-org";
    rev = "Use_dynamic_completion";
    sha256 = "0s3cfsy4563yfj37rvn9blm8dv80n7g9hxz5xybc1ww5vgc95zpl";
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
