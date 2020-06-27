{ stdenv
, python3Packages
, skidl
}:

python3Packages.buildPythonPackage rec {
  pname = "circlib";
  version = "0.1.0";
  src = /home/matt/src/circlib;

  propagatedBuildInputs = with python3Packages; [
    numpy
    setuptools
    matplotlib
    skidl
    pathos
  ];
}
