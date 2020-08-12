{ stdenv
# , fetchFromGitHub
, fetchgit
, python3Packages
# , python-csxcad
# , python-openems
}:

python3Packages.buildPythonPackage rec {
  pname = "pyems";
  version = "unstable-2020-08-10";
  src = /home/matt/src/pyems;
  # src = fetchgit {
  #   url = "file:///home/matt/src/pyems";
  #   rev = "02a601fe8d27367b427042d5eded517731d0d22c";
  #   # sha256 = "0000000000000000000000000000000000000000000000000000";
  # };
  # src = fetchFromGitHub {
  #   owner = "matthuszagh";
  #   repo = "pyems";
  #   rev = "c64cdb02378ba0c00cac654b5a77b3b7cd6c63b5";
  #   sha256 = "0hdq6wafkzh1cwj6hiqlklnf86v2m452f7rws8b4z8j44hnx5xw6";
  # };

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
