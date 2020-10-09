{ stdenv
, fetchFromGitHub
, buildPythonPackage
, numpy
, scipy
, matplotlib
, skidl
}:

buildPythonPackage rec {
  pname = "circlib";
  version = "unstable-2020-05-30";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "circlib";
    rev = "5ce6852b9c47754c0ee52136cbd6dce5a6b14fc6";
    sha256 = "0zba1wip951qfxa5rry1yrssgka17sp0cszpd1l993a50ms5ba2j";
  };

  propagatedBuildInputs = [
    numpy
    scipy
    matplotlib
    skidl
  ];

  meta = with stdenv.lib; {
    homepage = https://github.com/matthuszagh/circlib;
    description = "Part-aware circuit design using skidl and kicad";
    maintainers = with maintainers; [ matthuszagh ];
  };
}
