{ stdenv, fetchFromGitHub, libav }:

stdenv.mkDerivation rec {
  name = "avbin-${version}";
  version = "10";
  # src = /home/matt/src/AVbin;
  src = fetchFromGitHub {
    owner = "AVbin";
    repo = "AVbin";
    rev = "${name}";
    sha256 = "1vwxwb5b19y2j0lsgkq43nimn9vzafrbc57pcwg82l94b9a9sbz8";
  };

  # patches = [ ./avbin-shell.patch ];

  # builder = ./build.sh;
  buildInputs = [
    libav
  ];

  # buildPhase = ''
  #   cd $src/libav
  #   ./configure
  #   make
  #   # SRC_PATH=$src/libav make
  #   cd $src
  #   make PLATFORM=linux-x86-64 BUILDDIR=$(out)
  #   # $src/build.sh linux-x86-64
  # '';
  buildDate = "2019-07-29 09:12:58 -0700";
  buildFlags = [
    "PLATFORM=linux-x86-64"
    "BUILDDIR=$(out)"
    "BACKEND_DIR=${libav}"
    "AVBIN_VERSION_STRING=10"
    # "AVBIN_BUILD_DATE=\"$(date +%Y-%m-%d %H:%M:%S %z)\""
    # "AVBIN_BUILD_DATE=\"2019-07-29 09:12:58 -0700\""
    # "AVBIN_BUILD_DATE='${buildDate}'"
  ];
}
