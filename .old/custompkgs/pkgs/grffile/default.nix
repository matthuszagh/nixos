{ stdenv, fetchFromGitHub, texlive }:

stdenv.mkDerivation rec {
  version = "v2.1";
  pname = "grffile";
  tlType = "run";

  src = fetchFromGitHub {
    owner = "ho-tex";
    repo = "grffile";
    rev = version;
    sha256 = "0m1y7pw6xgrn1zxkj2rm5v3ia2b2fkjkya7wgvynagqflvm7h9rz";
  };

  buildInputs = [
    (texlive.combine { inherit (texlive) scheme-basic tex; })
  ];

  buildPhase = ''
    tex grffile.dtx
  '';

  installPhase = ''
    mkdir -p $out/tex/latex
    cp grffile.sty $out/tex/latex/
  '';

  meta = {
    platforms = stdenv.lib.platforms.linux;
  };
}
