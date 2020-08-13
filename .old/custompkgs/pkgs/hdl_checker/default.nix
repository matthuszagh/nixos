{ stdenv, fetchPypi, buildPythonPackage
, prettytable
, argcomplete
, python-language-server
, requests
, tabulate
, typing
, bottle
, waitress
}:

buildPythonPackage rec {
  pname = "hdl_checker";
  version = "0.6.12";

  src = fetchPypi {
    inherit pname version;
    sha256 = "08222k7iqw59v7a11clpqm69rjhidmcrapilipdngjfnsmn0rr8y";
  };

  patches = [
    ./requests.patch
    ./argparse.patch
  ];

  propagatedBuildInputs = [
    prettytable
    argcomplete
    python-language-server
    requests
    tabulate
    typing
    bottle
    waitress
  ];

  # TODO
  doCheck = false;

  meta = {
    description = "VHDL/Verilog/SystemVerilog language server for LSP";
    maintainers = with stdenv.lib.maintainers; [ matthuszagh ];
    license = with stdenv.lib.licenses; [ gpl3 ];
    homepage = https://github.com/suoto/hdl_checker;
  };
}
