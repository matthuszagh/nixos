{ stdenv
, lib
, fetchFromGitHub
, texlive
}:

stdenv.mkDerivation rec {
  pname = "qcircuitz";
  version = "unstable-2017-05-22";
  tlType = "run";

  src = fetchFromGitHub {
    owner = "CQuIC";
    repo = "qcircuitz";
    rev = "e9c2609fa8ea7cb66909cdb9c6067955afb321aa";
    sha256 = "02fzsg4nfasglq72crwc4hla9gn2y6lx0pjn05m5lvcd33427fhd";
  };

  dontBuild = true;

  installPhase = "
    mkdir -p $out/tex/latex
    cp qcircuit-tikz.sty $out/tex/latex/
  ";

  meta = {
    platforms = lib.platforms.linux;
  };
}
