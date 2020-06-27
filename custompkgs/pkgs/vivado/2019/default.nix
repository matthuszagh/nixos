{ stdenv, coreutils, fetchurl, patchelf, procps, makeWrapper, requireFile
, ncurses5, zlib, libuuid, libSM, libICE, libX11, libXrender
, libxcb, libXext, libXtst, libXi, glib, freetype, gtk2
, openjdk
}:

stdenv.mkDerivation rec {
  name = "vivado-2019-1";

  buildInputs = [
    patchelf
    procps
    ncurses5
    zlib
    makeWrapper
    coreutils
  ];

  builder = ./builder.sh;
  inherit ncurses5;

  # requireFile prevents rehashing each time, which saves time during
  # rebuilds.
  src = requireFile rec {
    name = "Xilinx_Vivado_SDK_2019.1_0524_1430.tar.gz";
    message = ''
      This nix expression requires that ${name} is already part of the store.
      Login to Xilinx, download from
      https://www.xilinx.com/support/download.html,
      rename the file to ${name}, and add it to the nix store with
      "nix-prefetch-url file:///path/to/${name}".
    '';
    sha256 = "0a60fqyrfj0d8wcjlqi2mmi320r3xilndppk16isnddwihd0iczj";
  };

  libPath = stdenv.lib.makeLibraryPath [
    stdenv.cc.cc
    ncurses5
    openjdk
    zlib
    libuuid
    libSM
    libICE
    libX11
    libXrender
    libxcb
    libXext
    libXtst
    libXi
    glib
    freetype
    gtk2
  ];

  meta = {
    description = "Xilinx Vivado";
    homepage = "https://www.xilinx.com/products/design-tools/vivado.html";
    license = stdenv.lib.licenses.unfree;
  };
}
