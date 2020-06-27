{ stdenv, pkgs }:

# See https://orgmode.org/worg/dev/org-build-system.html for how to
# build
stdenv.mkDerivation rec {
  name = "org-mode";
  src = /home/matt/src/org-mode;

  propagatedBuildInputs = with pkgs; [
    emacs
    git
    texinfo
    tree
  ];

  buildPhase = ''
    make
  '';

  installPhase = ''
    echo $out
    install -D -t $out/share/emacs/site-lisp lisp/*.el*
    install -D -t $out/share/info doc/org
  '';

  meta = {
    description = "Org Mode";
  };
}
