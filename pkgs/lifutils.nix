{ stdenv
, lib
, fetchFromGitHub
, cmake
}:

stdenv.mkDerivation {
  pname = "lifutils";
  version = "unstable-2020-06-05";

  nativeBuildInputs = [ cmake ];

  postPatch = ''
    sed -i 's@set(CMAKE_INSTALL_PREFIX "/usr")@@g' CMakeLists.txt
  '';

  src = fetchFromGitHub {
    owner = "bug400";
    repo = "lifutils";
    rev = "1736f842de20c8216b25aef9e1e97bdeb7ce6d65";
    sha256 = "sha256-m1/RafPjXGrIr+8QNrUArFNhkb3jQI3i2ahOnIUrQOs=";
  };
}
