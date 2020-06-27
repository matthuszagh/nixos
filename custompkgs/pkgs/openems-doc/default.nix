{ stdenv, fetchFromGitHub
, python
, texinfo
, openems
, csxcad
}:

stdenv.mkDerivation rec {
  pname = "openems-doc";
  version = "0.35.0";

  src = fetchFromGitHub {
    owner = "thliebig";
    repo = "openEMS-Project";
    rev = "78e76425ba59c331d30f04647b5a1d05206f47d7";
    sha256 = "0dsxr7i4q24yjg8418gpn80klmb25dw4g7a86g0jdqcirn6gpa4y";
  };

  buildInputs = [
    python
    texinfo
  ];

  buildPhase = ''
    make -e BUILDDIR=$(pwd)/_build -C $src/doc info
    make -e BUILDDIR=$(pwd)/_build -C $src/doc man
  '';

  installPhase = ''
    mkdir -p $out/share/info && cp -r _build/texinfo/*.info $out/share/info
    mkdir -p $out/share/man && cp -r _build/man/man $out/share/man
  '';
}
