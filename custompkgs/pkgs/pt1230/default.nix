{ fetchFromGitHub, stdenv, freetype, fontconfig, pkgconfig }:

stdenv.mkDerivation rec {
  name = "pt1230-${version}";
  version = "c934eaa7fbeec731d26cce2df5751784fe5b7611";
  src = fetchFromGitHub {
    owner = "cbdevnet";
    repo = "pt1230";
    rev = "c934eaa7fbeec731d26cce2df5751784fe5b7611";
    sha256 = "0nh0g23z4s2vvhsvfx7317226hlx9dhv21vhzng1n4iz54h3j2z8";
  };

  nativeBuildInputs = [ freetype fontconfig pkgconfig ];

  enableParallelBuilding = true;

  installPhase = ''
    mkdir -p $out/bin
    install -m 0755 pt1230 $out/bin/
    install -m 0755 textlabel $out/bin/
    install -m 0755 line2bitmap $out/bin/
  '';

  meta = {
    description = "Brother P-Touch 1230PC Linux interface";
    homepage = https://github.com/cbdevnet/pt1230;
    platforms = stdenv.lib.platforms.linux;
  };
}
