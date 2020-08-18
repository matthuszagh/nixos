{ stdenv
, fetchFromGitHub
, texlive
}:

stdenv.mkDerivation rec {
  pname = "grffile";
  version = "2.1";
  tlType = "run";

  src = fetchFromGitHub {
    owner = "ho-tex";
    repo = "grffile";
    rev = "v${version}";
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
