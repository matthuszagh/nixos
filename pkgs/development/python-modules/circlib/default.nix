{ stdenv
, buildPythonPackage
, numpy
, matplotlib
, skidl
, pathos
}:

buildPythonPackage rec {
  pname = "circlib";
  version = "0.1.0";
  src = /home/matt/src/circlib;

  propagatedBuildInputs = [
    numpy
    matplotlib
    skidl
    pathos
  ];
}
