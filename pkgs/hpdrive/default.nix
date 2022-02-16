{ lib
, stdenv
, fetchurl
# TODO rename to linux-gpib
, linux-gpib-user
, patchelf
}:

stdenv.mkDerivation rec {
  pname = "hpdrive";
  version = "40rc2";
  # version = "4.0-rc2.1";

  src = fetchurl {
    url = "hp9845.net/tmp/dist64.tgz";
    # url = "https://hp9845.net/tmp/binaries.tgz";
    sha256 = "sha256-/bcfiaJp4Wb3gDwRtdbR6KxqFfrcIxU7F2nboibB3DY=";
    # sha256 = "sha256-YtZ4c/G09HtOeG2JGSzWyeWPnwcnB/9NeY4BjIlCIUk=";
  };

  unpackPhase = ''
    tar xf $src
  '';

  buildInputs = [
    # TODO rename to linux-gpib
    linux-gpib-user
  ];

  nativeBuildInputs = [ patchelf ];

  # TODO rename to linux-gpib
  libpath = lib.makeLibraryPath [ linux-gpib-user ];

  # outputs = [ "bin" ];

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
