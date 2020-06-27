{ stdenv, fetchFromGitHub, python3Packages, yosys, symbiyosys, yices }:

python3Packages.buildPythonPackage {
  name = "nmigen";

  src = fetchFromGitHub {
    owner = "m-labs";
    repo = "nmigen";
    rev = "ad1a40c934cc33374109069296386d66023671f1";
    sha256 = "0g1krym3zip0b5gwkfw51qb759l92nnnfwjbkvny8gyfmlwzmywk";
  };

  # checkPhase = "PATH=${yosys}/bin:${symbiyosys}/bin:${yices}/bin:$PATH python -m unittest discover nmigen.test";
  checkPhase = "PATH=${yosys}/bin:${symbiyosys}/bin:${yices}/bin:$PATH";

  propagatedBuildInputs = [ python3Packages.bitarray python3Packages.pyvcd python3Packages.jinja2 ];

  meta = with stdenv.lib; {
    description = "A refreshed Python toolbox for building complex digital hardware";
    homepage    = "https://lambdaconcept.com";
    license     = licenses.bsd2;
    maintainers = [ maintainers.sb0 ];
  };
}
