{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  name = "primerun";
  src = fetchFromGitHub {
    owner = "dukzcry";
    repo = "crap";
    rev = "57a2039";
    sha256 = "0i73fkkjzv4hy7rm12c9g66qlj9qxv78qq18g2sw3hznsd211fb5";
  };

  patches = [ ./primerun.patch ];
  makefile = [ ./Makefile ];
  makeFlags = [ "PREFIX=$(out)" ];
}
