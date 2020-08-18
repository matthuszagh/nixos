{ stdenv
, fetchPypi
, buildPythonApplication
, prettytable
, argcomplete
, python-language-server
, requests
, tabulate
, bottle
, waitress
}:

buildPythonApplication rec {
  pname = "hdl_checker";
  version = "0.6.14";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0pm7sf91ml72pah6napi53d977hsn283xa5iw1blp6g8vp20gj45";
  };

  patches = [
    ./setup.patch
  ];

  propagatedBuildInputs = [
    prettytable
    argcomplete
    python-language-server
    requests
    tabulate
    bottle
    waitress
  ];

  doCheck = false;

  meta = {
    description = "VHDL/Verilog/SystemVerilog language server for LSP";
    maintainers = with stdenv.lib.maintainers; [ matthuszagh ];
    license = with stdenv.lib.licenses; [ gpl3 ];
    homepage = https://github.com/suoto/hdl_checker;
  };
}
