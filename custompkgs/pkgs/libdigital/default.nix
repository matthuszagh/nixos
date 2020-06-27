{ stdenv, python3Packages, cocotb }:

python3Packages.buildPythonPackage rec {
  pname = "libdigital";
  version = "0.1.0";
  src = /home/matt/src/libdigital;

  propagatedBuildInputs = with python3Packages; [
    nmigen
    numpy
    bitstring
    setuptools
    matplotlib
    scipy
    cocotb
  ];

  # meta = with stdenv.lib; {
  #   description = "A refreshed Python toolbox for building complex digital hardware";
  #   homepage    = "https://lambdaconcept.com";
  #   license     = licenses.bsd2;
  #   maintainers = [ maintainers.sb0 ];
  # };
}
