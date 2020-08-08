{ stdenv
, fetchFromGitHub
, python3Packages
, python-csxcad
, python-openems
}:

python3Packages.buildPythonPackage rec {
  pname = "pyems";
  version = "0.1.0";
  # src = /home/matt/src/pyems;
  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "pyems";
    rev = "c64cdb02378ba0c00cac654b5a77b3b7cd6c63b5";
    sha256 = "0hdq6wafkzh1cwj6hiqlklnf86v2m452f7rws8b4z8j44hnx5xw6";
  };

  # buildInputs = with python3Packages; [
  #   setuptools
  # ];

  propagatedBuildInputs = with python3Packages; [
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
