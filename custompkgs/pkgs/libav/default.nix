{ stdenv, fetchFromGitHub, yasm }:

stdenv.mkDerivation rec {
  name = "libav-v${version}";
  version = "12.3";
  src = fetchFromGitHub {
    owner = "libav";
    repo = "libav";
    rev = "v${version}";
    sha256 = "1dcxc7hyxvx98ab0j04r4ivbhw44l0l2mfk6d3ymhwvmvqjy8dna";
  };

  configureFlags = [ "--enable-shared" ];
  buildInputs = [
    yasm
  ];
}
