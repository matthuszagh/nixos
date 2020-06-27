{ stdenv
# , fetchFromGitHub
# , fetchPypi
, python3Packages
, libngspice
}:

python3Packages.buildPythonPackage rec {
  pname = "pyspice";
  version = "1.4.2";
  # version = "1.2";

  # src = /home/matt/src/PySpice;
  # src = fetchFromGitHub {
  #   owner = "FabriceSalvaire";
  #   repo = "PySpice";
  #   rev = "c0dc504b65bed3aad52d06d6dbe06844ade7a7ce";
  #   sha256 = "1k11dl1kfxji70zl5a7s9pchdri4dszxdrlmslp5ghw8296wyfvb";
  # };
  src = stdenv.lib.fetchPypi {
    inherit pname version;
  };

  # propagatedBuildInputs = (with python3Packages; [
  #   setuptools
  #   pyyaml
  #   cffi
  #   matplotlib
  #   numpy
  #   ply
  #   scipy
  # ]) ++ [
  #   libngspice
  # ];

  # # # TODO fix this
  # # doCheck = false;
  # checkInputs = with python3Packages; [
  #   pytest
  # ];

  # # TODO this is a hack
  # postPatch = ''
  #   cp PySpice/__init__.py.in PySpice/__init__.py
  #   substituteInPlace ngspice-shared/test.py --replace "ffi.dlopen(ngspice_shared_path)" "ffi.dlopen('${libngspice}/lib/libngspice.so')"
  #   substituteInPlace PySpice/Spice/NgSpice/Shared.py --replace "ffi.dlopen(library_path)" "ffi.dlopen('${libngspice}/lib/libngspice.so')"
  # '';

  meta = with stdenv.lib; {
    description = "Simulate electronic circuit using Python and the Ngspice / Xyce simulators";
    homepage = "https://github.com/FabriceSalvaire/PySpice";
    license = licenses.gpl3;
    maintainers = with maintainers; [ matthuszagh ];
  };
}
