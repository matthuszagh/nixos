{ stdenv
, fetchurl
}:

stdenv.mkDerivation rec {
  pname = "linux-gpib";
  version = "4.3.4";

  src = fetchurl {
    url = "https://sourceforge.net/projects/linux-gpib/files/linux-gpib%20for%203.x.x%20and%202.6.x%20kernels/${version}/${pname}-${version}.tar.gz";
    sha256 = "sha256-NWA4WvnlIMpyBZEDlUjKhmwLTcKab6CbpIxTdtsfMM4=";
  };

  unpackPhase = ''
    tar xf $src
    tar xf ${pname}-${version}/${pname}-user-${version}.tar.gz
  '';

  sourceRoot = "${pname}-user-${version}";

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

  configurePhase = ''
    ./configure --sysconfdir=$out/etc --prefix=$out
  '';

  # TODO have special linux-gpib packages (kernel and user) for
  # hpdrive. Name them something like -patched-hpdrive.
  patches = [
    ./linux-gpib-user-4.3.3.patch
  ];
}
