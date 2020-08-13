{ stdenv, python3Packages, fetchFromGitHub
# , kicad
}:

python3Packages.buildPythonPackage rec {
  pname = "pykicad";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner = "dvc94ch";
    repo = "pykicad";
    rev = "c1e922f7250e4fb5711b135df837980c2bdde333";
    sha256 = "0zxpm4cb2r03jmhd4bxx6y6j50d95xr35ibz2xaq811yrsmb0j0d";
  };

  doCheck = false;

  checkInputs = with python3Packages; [
    pytest
  ];

  propagatedBuildInputs = with python3Packages; [
    setuptools
    pyparsing
    # kicad
  ];

  meta = with stdenv.lib; {
    description = "Library for working with KiCAD file formats";
    homepage = "https://github.com/dvc94ch/pykicad";
    license = licenses.isc;
    maintainers = with maintainers; [ matthuszagh ];
  };
}
