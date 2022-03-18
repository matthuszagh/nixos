{ stdenv
, fetchurl
, lib
, libX11
, gtk2-x11
, pango
, gdk-pixbuf
, cairo
, atk
, expat
}:

stdenv.mkDerivation rec {
  pname = "scsi2sd-util6";
  version = "unstable-2021-05-27";

  src = fetchurl {
    url = "http://www.codesrc.com/files/scsi2sd-v6/latest/linux/scsi2sd-util6";
    sha256 = "sha256-J0Ja+GIPpbOIHfMR4bR+sK76ZQ1YLCHcFW7j1p4m1jM=";
  };

  dontUnpack = true;

  # buildInputs = [
  #   libX11
  # ];

  libpath = lib.makeLibraryPath [
    libX11
    gtk2-x11
    pango
    gdk-pixbuf
    cairo
    atk
    expat
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/scsi2sd-util6
    chmod +x $out/bin/scsi2sd-util6
  '';

  postFixup = ''
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath ${libpath} \
      $out/bin/scsi2sd-util6
  '';

  meta = with lib; {
    description = "Utilities for SCSI2SD V6.";
    homepage = "http://www.codesrc.com/mediawiki/index.php/SCSI2SD";
    license = licenses.unfree;
    maintainers = with maintainers; [ matthuszagh ];
    platforms = platforms.linux;
  };
}
