{ stdenv
, lib
, fetchFromGitHub
}:

stdenv.mkDerivation {
  pname = "disk-utilities";
  version = "unstable-2022-01-28";

  src = fetchFromGitHub {
    owner = "keirf";
    repo = "Disk-Utilities";
    rev = "ab0e448e382c19969c6a1ea420f8593079857864";
    sha256 = "sha256-9jM7+O6JdCWeowCyUzVVIWVekHoZyL+ySuDhw0nvgzQ=";
  };

  installPhase = ''
    mkdir $out
    make install PREFIX=$out
  '';

  meta = with lib; {
    description = "A collection of utilities for ripping, dumping, analysing, and modifying disk images.";
    homepage = "https://github.com/keirf/disk-utilities";
    license = licenses.unlicense;
    maintainers = with maintainers; [ matthuszagh ];
    platforms = platforms.linux;
  };
}
