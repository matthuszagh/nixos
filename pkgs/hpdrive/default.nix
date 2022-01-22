{ lib
, stdenv
, fetchurl
, linux-gpib
}:

stdenv.mkDerivation rec {
  pname = "hpdrive";
  version = "40rc2";

  src = fetchurl {
    url = "hp9845.net/tmp/dist64.tgz";
    sha256 = "sha256-/bcfiaJp4Wb3gDwRtdbR6KxqFfrcIxU7F2nboibB3DY=";
  };

  unpackPhase = ''
    tar xf $src
  '';

  buildInputs = [
    linux-gpib
  ];

  libpath = lib.makeLibraryPath [ linux-gpib ];

  installPhase = ''
    mkdir -p $out/bin
    cp hpdir $out/bin/
    cp hpdrive $out/bin/
    cp hpprint $out/bin/
  '';

  postFixup = ''
    for file in $out/bin/*; do
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath ${libpath} \
        $file
    done
  '';
}
