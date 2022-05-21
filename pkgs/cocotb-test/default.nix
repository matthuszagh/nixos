{ lib
, stdenv
, buildPythonPackage
, fetchPypi
, pytest
, cocotb
}:

buildPythonPackage rec {
  pname = "cocotb-test";
  version = "0.2.2";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-xcT+L7gesVv8gwxsWNbouBktOgMTKxQ/F+++bAVuO8c=";
  };

  buildInputs = [
    pytest
    cocotb
  ];

  pythonImportsCheck = [ "cocotb_test" ];

  meta = with lib; {
    description = "Unit testing for cocotb";
    homepage = "https://github.com/themperek/cocotb-test";
    license = licenses.bsd2;
    maintainers = with maintainers; [ matthuszagh ];
  };
}
