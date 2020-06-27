{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "hal";
  version = "1.1.5";
  src = fetchFromGitHub {
    owner = "emsec";
    repo = "hal";
    rev = "v${version}";
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };
}
