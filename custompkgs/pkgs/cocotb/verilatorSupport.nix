{ fetchFromGitHub, stdenv
, python3Packages
}:

python3Packages.buildPythonPackage rec {
  name = "cocotb";
  src = fetchFromGitHub {
    owner = "antmicro";
    repo = "cocotb-verilator-build";
    rev = "b9f6d0bd1d369cf45ab51b818812f94f01b31dc6";
    sha256 = "1cr68ccnjrrkxdg9c46sanqkdgyw5hrjasn6pmwjapv2r252fail";
    fetchSubmodules = true;
  };

  # meta = {
  #   description = "cocotb is a coroutine based cosimulation library for writing VHDL and Verilog testbenches in Python";
  #   homepage = https://github.com/antmicro/cocotb;
  #   license = stdenv.lib.licenses.lgpl3;
  #   maintainers = [ ];
  #   platforms = stdenv.lib.platforms.linux;
  # };
}
