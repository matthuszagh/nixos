{ stdenv
, fetchFromGitHub
, buildPythonPackage
, numpy
, scipy
, python-openems
, python-csxcad
}:

buildPythonPackage rec {
  pname = "pyems";
  version = "unstable-2020-08-18";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "pyems";
    rev = "556f049d38aa69b111b13bcf6cd637abe69e2b40";
    sha256 = "0rcwcs2nm5sr1q3x3c8zffd093iay18lpqbfmmvqy2dnw8rkmg7v";
  };

  propagatedBuildInputs = [
    numpy
    scipy
    python-openems
    python-csxcad
  ];

  meta = with stdenv.lib; {
    homepage = https://github.com/matthuszagh/pyems;
    description = "High-level python interface to OpenEMS with automatic mesh generation";
    license = licenses.gpl3;
    maintainers = with maintainers; [ matthuszagh ];
  };
}
