{ stdenv, python3Packages, fetchFromGitHub, swig, verilog }:

python3Packages.buildPythonPackage rec {
   pname = "cocotb";
   version = "1.2.0";

   src = fetchFromGitHub {
     owner = pname;
     repo = pname;
     rev = "v${version}";
     sha256 = "091q63jcm87xggqgqi44lw2vjxhl1v4yl0mv2c76hgavb29w4w5y";
   };

   propagatedBuildInputs = with python3Packages; [
     setuptools
   ];

   postPatch = ''
     patchShebangs bin/*.py

     # POSIX portability (TODO: upstream this)
     for f in \
       cocotb/share/makefiles/Makefile.* \
       cocotb/share/makefiles/simulators/Makefile.*
     do
       substituteInPlace $f --replace 'shell which' 'shell command -v'
     done
   '';

   checkInputs = [ swig verilog ];

   checkPhase = ''
     export PATH=$out/bin:$PATH
     make test
   '';

   meta = with stdenv.lib; {
     description = "Coroutine based cosimulation library for writing VHDL and Verilog testbenches in Python";
     homepage = https://github.com/cocotb/cocotb;
     license = licenses.bsd3;
     maintainers = with maintainers; [ matthuszagh ];
   };
 }
