{ stdenv
, fetchFromGitHub
, buildPythonPackage
, numpy
, matplotlib
, skidl
, pathos
}:

buildPythonPackage rec {
  pname = "circlib";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "circlib";
    rev = "5ce6852b9c47754c0ee52136cbd6dce5a6b14fc6";
    sha256 = "0zba1wip951qfxa5rry1yrssgka17sp0cszpd1l993a50ms5ba2j";
  };

  propagatedBuildInputs = [
    numpy
    matplotlib
    skidl
    pathos
  ];
}
