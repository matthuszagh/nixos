{ stdenv
, fetchurl
, fetchsvn
, autoconf
, libtool
, yacc
, flex
, automake
}:

stdenv.mkDerivation rec {
  pname = "linux-gpib";
  # version = "4.3.4";
  version = "4.4.0";

  src = fetchurl {
    # url = "https://sourceforge.net/projects/linux-gpib/files/linux-gpib%20for%203.x.x%20and%202.6.x%20kernels/${version}/${pname}-${version}.tar.gz";
    # sha256 = "sha256-NWA4WvnlIMpyBZEDlUjKhmwLTcKab6CbpIxTdtsfMM4=";

    # patched (for hpdrive) version of linux-gpib-4.4.0
    url = "https://hp9845.net/tmp/linux-gpib-4.4.0.tar.gz";
    sha256 = "sha256-EgLbHE9gLNDAcvK3jozkOpOEXrXFFv7WZufchnpa4SY=";
  };

  # src = fetchsvn {
  #   url = "https://svn.code.sf.net/p/linux-gpib/code/trunk";
  #   sha256 = "sha256-tZmbxxQGhqu2hYLrHFqMzUJlrA81D3fhUsW1A1iGJ2Q=";
  #   rev = "1984";
  # };

  unpackPhase = ''
    tar xf $src
    tar xf ${pname}-${version}/${pname}-user-${version}.tar.gz
  '';

  sourceRoot = "${pname}-user-${version}";

  # postUnpack = ''
  #   rm -r $sourceRoot/linux-gpib-kernel
  #   mv $sourceRoot/linux-gpib-user/* $sourceRoot/.
  #   rm -r $sourceRoot/linux-gpib-user
  # '';

  # TODO shouldn't be necessary, but do it for now since I can't get
  # linux-gpib to recognize another config file.
  postPatch = ''
    rm util/templates/gpib.conf

    echo "interface {
    minor = 0
    board_type = "ni_pci"
    name = "gpib0"
    pad = 21
    master = yes
    pci_bus = 7
    pci_slot = 12
}" > util/templates/gpib.conf
  '';

  # nativeBuildInputs = [
  #   autoconf
  #   libtool
  #   yacc
  #   flex
  #   automake
  # ];

  # preConfigure = ''
  #   ./bootstrap
  #   ls
  # '';

  configurePhase = ''
    # ./bootstrap
    ./configure --sysconfdir=$out/etc --prefix=$out
  '';

  # TODO shouldn't be needed
  postInstall = ''
    ln -s $out/lib/libgpib.so $out/lib/libgpib.so.0
  '';

  # # TODO have special linux-gpib packages (kernel and user) for
  # # hpdrive. Name them something like -patched-hpdrive.
  # patches = [
  #   ./linux-gpib-user-4.3.3.patch
  # ];
}
