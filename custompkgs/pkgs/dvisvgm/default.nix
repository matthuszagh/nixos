{ stdenv, fetchFromGitHub, pkgconfig
, autoreconfHook
, texlive-bin
, ghostscript
, brotli
, woff2
, zlib
, freetype
, potrace
, xxHash
}:

let
  withSystemLibs = map (libname: "--with-system-${libname}");
in
stdenv.mkDerivation rec {
  version = "2.8.1";
  pname = "dvisvgm";
  tlType = "bin";

  src = fetchFromGitHub {
    owner = "mgieseki";
    repo = "dvisvgm";
    rev = version;
    sha256 = "0bnqqca15zdvz2rrjnkmnwgrr0d6hxs736ibw26588bpz54mag5i";
  };

  nativeBuildInputs = [ autoreconfHook pkgconfig ];
  # TODO: dvisvgm still uses vendored dependencies
  propagatedBuildInputs = [
    texlive-bin
    ghostscript
    brotli
    woff2
    zlib
    freetype
    potrace
    xxHash
  ];

  configureFlags = [
    "--with-kpathsea=${texlive-bin}"
  ];

  enableParallelBuilding = true;

  meta = {
    platforms = stdenv.lib.platforms.linux;
  };
}
