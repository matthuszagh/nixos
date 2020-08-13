{ stdenv, fetchFromGitHub
, csxcad, fparser, tinyxml, hdf5, vtk, boost, cgal, zlib, cmake, octave, gl2ps
, withQcsxcad ? true
, withMPI ? false
, withHyp2mat ? true
, qcsxcad ? null
, openmpi ? null
, hyp2mat ? null
}:

assert withQcsxcad -> qcsxcad != null;
assert withMPI -> openmpi != null;
assert withHyp2mat -> hyp2mat != null;

stdenv.mkDerivation rec {
  pname = "openems";
  version = "master";

  src = fetchFromGitHub {
    owner = "thliebig";
    repo = "openEMS";
    rev = "9c78459d5499e40d2e80c2cbfe1da62f40918997";
    sha256 = "0dk49l3w9wrmn6zksyp1avgi5x80y72cfzrvw3qkak8dsd3ghbh7";
  };

  nativeBuildInputs = [
    cmake
  ];

  cmakeFlags = stdenv.lib.optionals withMPI [ "-DWITH_MPI=ON" ];

  buildInputs = [
    fparser
    tinyxml
    hdf5
    vtk
    boost
    cgal
    zlib
    csxcad
    (octave.override { inherit gl2ps hdf5; })
    (if withQcsxcad then qcsxcad else null)
    (if withMPI then (openmpi.override { enableCpp = true; }) else null)
    (if withHyp2mat then hyp2mat else null)
  ];

  postFixup = ''
    substituteInPlace $out/share/openEMS/matlab/setup.m \
      --replace /usr/lib ${hdf5}/lib \
      --replace /usr/include ${hdf5}/include

    ${octave}/bin/mkoctfile -L${hdf5}/lib -I${hdf5}/include \
      -lhdf5 $out/share/openEMS/matlab/h5readatt_octave.cc \
      -o $out/share/openEMS/matlab/h5readatt_octave.oct
  '';

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "Open Source Electromagnetic Field Solver";
    homepage = http://openems.de/index.php/Main_Page.html;
    license = licenses.gpl3;
    maintainers = with maintainers; [ matthuszagh ];
    platforms = platforms.linux;
  };
}
