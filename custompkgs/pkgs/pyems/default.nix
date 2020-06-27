{ stdenv, python3Packages
, python-csxcad
, python-openems
}:

python3Packages.buildPythonPackage rec {
  pname = "pyems";
  version = "0.1.0";
  src = /home/matt/src/pyems;

  propagatedBuildInputs = with python3Packages; [
    numpy
    scipy
    setuptools
    pathos
    python-openems
    python-csxcad
  ];
}
