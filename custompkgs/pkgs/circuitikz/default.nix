{ stdenv
, fetchFromGitHub
, texlive
}:

stdenv.mkDerivation rec {
  version = "v1.2.2";
  pname = "circuitikz";
  tlType = "run";

  src = fetchFromGitHub {
    owner = "circuitikz";
    repo = "circuitikz";
    rev = version;
    sha256 = "1sbp2pj5qi6i7x8zj2x8ibh6fybjqx7mxm974m1gs3np8agn11fx";
  };

  dontBuild = true;

  installPhase = "
    mkdir -p $out/tex/latex
    cp tex/* $out/tex/latex/
  ";

  meta = {
    platforms = stdenv.lib.platforms.linux;
  };
}
