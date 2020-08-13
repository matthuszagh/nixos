{ stdenv, fetchFromGitHub, emacs }:

stdenv.mkDerivation rec {
  name = "justify-kp";

  src = fetchFromGitHub {
    owner = "Fuco1";
    repo = "justify-kp";
    rev = "master";
    sha256 = "0cmf2m9ingvffj399pqpxbjl76a3wh6822v4lx6p3nc6ffvk8chw";
  };

  buildInputs = [
    emacs
  ];

  # buildPhase = ''
  #   emacs -L . --batch -f batch-byte-compile *.el
  # '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    install *.el* $out/share/emacs/site-lisp
  '';

  meta = {
    description = "Paragraph justification for emacs using Knuth/Plass algorithm";
    homepage = https://github.com/Fuco1/justify-kp;
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.all;
  };
}
