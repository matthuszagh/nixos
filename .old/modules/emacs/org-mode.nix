{ pkgs
, emacs
}:

# See https://orgmode.org/worg/dev/org-build-system.html for how to
# build
pkgs.stdenv.mkDerivation rec {
  pname = "org";
  version = "src";
  src = /home/matt/src/org-mode;

  propagatedBuildInputs = [
    emacs
  ] ++ (with pkgs; [
    git
    texinfo
    tree
  ]);

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
