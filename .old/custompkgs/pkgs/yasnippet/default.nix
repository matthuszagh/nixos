{ stdenv, fetchFromGitHub, emacs, pkgs }:

stdenv.mkDerivation rec {
  name = "yasnippet";

  src = fetchFromGitHub {
    owner = "Zetagon";
    repo = "yasnippet";
    rev = "master";
    sha256 = "1nlvmicxh1vhd77615ykp6ss8h19k3axipfibifvcdgh3xr3bdwn";
  };

  buildInputs = [
    emacs
  ];

  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    install *.el* $out/share/emacs/site-lisp
  '';
}
