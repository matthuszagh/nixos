{ fetchFromGitHub, stdenv, cmake
, csxcad, tinyxml, vtkWithQt4
, qt4
}:

stdenv.mkDerivation rec {
  name = "qcsxcad-${version}";
  version = "0.6.2";
  src = fetchFromGitHub {
    owner = "thliebig";
    repo = "QCSXCAD";
    rev = "v${version}";
    sha256 = "1l77xr1skf9rrd0pwdw0wcz8a3qzwnn9vz7613j2w5xwakagiglj";
  };

  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    "-DCMAKE_INSTALL_PREFIX=${placeholder "out"}"
    "-DCSXCAD_ROOT_DIR=${csxcad}"
    "-DENABLE_RPATH=OFF"
  ];

  buildInputs = [
    csxcad
    tinyxml
    vtkWithQt4
    qt4
  ];

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "Qt-GUI for CSXCAD";
    homepage = https://github.com/thliebig/QCSXCAD;
    license = licenses.gpl3;
    maintainers = with maintainers; [ matthuszagh ];
    platforms = platforms.linux;
  };
}
