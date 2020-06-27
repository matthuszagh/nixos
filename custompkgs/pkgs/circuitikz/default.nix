{ stdenv, fetchFromGitHub, texlive }:

stdenv.mkDerivation rec {
  version = "circuitikz-unstable-2020-05-30";
  pname = "circuitikz";
  tlType = "run";

  src = fetchFromGitHub {
    owner = "circuitikz";
    repo = "circuitikz";
    rev = "b908eb8ecc62967d2a48228a670e893e1a06302e";
    sha256 = "0x5vmzjk58sa8arczwrp3b1ln65s0904xsp6vdy1pfzkmz95fqhf";
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
