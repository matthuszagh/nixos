{ stdenv, python3Packages, fetchFromGitHub
# , kicad
}:

python3Packages.buildPythonPackage rec {
  pname = "kinparse";
  version = "0.1.2";

  src = fetchFromGitHub {
    owner = "xesscorp";
    repo = "kinparse";
    rev = "1ad470beb638f25975e9d44a1177f241944faad2";
    sha256 = "0kk4d3fbkjmd3hljjd2p4zhlcx8ybljjkpnhkzykiiy98j0k5nby";
  };

  doCheck = true;

  checkInputs = with python3Packages; [
    pytest
  ];

  propagatedBuildInputs = with python3Packages; [
    setuptools
    future
    pyparsing
    # kicad
  ];

  meta = with stdenv.lib; {
    description = "A Parser for KiCad EESCHEMA netlists";
    homepage = "https://github.com/xesscorp/kinparse";
    license = licenses.mit;
    maintainers = with maintainers; [ matthuszagh ];
  };
}
