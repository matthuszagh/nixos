{ stdenv, fetchFromGitHub
, texlive
, lua5_3
}:

stdenv.mkDerivation rec {
  version = "1.11.1-svn7215";
  pname = "luatex";
  tlType = "bin";

  # official luatex mirror
  src = fetchFromGitHub {
    owner = "TeX-Live";
    repo = "luatex";
    rev = "${version}";
    sha256 = "11pkhqprkm9y456ndi1ax705n1083zkjhzqk86r2a4dmwx3qb62d";
  };

  buildInputs = [
    lua5_3
  ];

  buildPhase = ''
    sh $src/build.sh --parallel
  '';

  installPhase = "
    mkdir -p $out/bin
    cp build/texk/web2c/luatex $out/bin/luatex
  ";

  meta = {
    platforms = stdenv.lib.platforms.linux;
  };
}
