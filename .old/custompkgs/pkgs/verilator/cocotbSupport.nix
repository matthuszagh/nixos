{ fetchFromGitHub, stdenv
}:

stdenv.mkDerivation rec {
  name = "verilator";
  src = fetchFromGitHub {
    owner = "antmicro";
    repo = "verilator";
    rev = "7c47f66a7e892567bf3e33fb423b743dff44ae4e";
    sha256 = "1205xgkjz95rxy9rf57y751p69qdicdawygarr63pfn6dih7hyv2";
  };

  # meta = {
  #   description = "cocotb is a coroutine based cosimulation library for writing VHDL and Verilog testbenches in Python";
  #   homepage = https://github.com/antmicro/cocotb;
  #   license = stdenv.lib.licenses.lgpl3;
  #   maintainers = [ ];
  #   platforms = stdenv.lib.platforms.linux;
  # };
}
