{ fetchFromGitHub, stdenv, cmake
, csxcad, qcsxcad, hdf5, vtk, qt4, fparser, tinyxml, cgal, boost
}:

stdenv.mkDerivation rec {
  name = "appcsxcad-${version}";
  version = "0.2.2";
  src = fetchFromGitHub {
    owner = "thliebig";
    repo = "AppCSXCAD";
    rev = "v${version}";
    sha256 = "05xj7xmbsdhasycyyax1m742sjzwp2vzndwiaz966a2pam2fxi9c";
  };

  buildInputs = [
    csxcad
    qcsxcad
    hdf5
    vtk
    qt4
    fparser
    tinyxml
    cgal
    boost
  ];

  nativeBuildInputs = [ cmake ];

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "Minimal Application using the QCSXCAD library";
    homepage = https://github.com/thliebig/AppCSXCAD;
    license = licenses.gpl3;
    maintainers = with maintainers; [ matthuszagh ];
    platforms = platforms.linux;
  };
}
