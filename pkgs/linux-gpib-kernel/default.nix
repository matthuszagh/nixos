{ stdenv
, fetchurl
, kernel
}:

stdenv.mkDerivation rec {
  pname = "linux-gpib";
  version = "4.3.4";

  src = fetchurl {
    url = "https://sourceforge.net/projects/linux-gpib/files/linux-gpib%20for%203.x.x%20and%202.6.x%20kernels/${version}/${pname}-${version}.tar.gz";
    sha256 = "sha256-NWA4WvnlIMpyBZEDlUjKhmwLTcKab6CbpIxTdtsfMM4=";
  };

  buildInputs = kernel.moduleBuildDependencies;

  kernelVersion = kernel.dev.modDirVersion;
  kernelSrc = "${kernel.dev}/lib/modules/${kernelVersion}/build";

  unpackPhase = ''
    tar xf $src
    tar xf ${pname}-${version}/${pname}-kernel-${version}.tar.gz
  '';

  patches = [
    ./linux-gpib-kernel-4.3.4.patch
  ];

  postPatch = ''
    sed -i 's@/sbin/depmod -A@@g' Makefile
  '';

  sourceRoot = "${pname}-kernel-${version}";

  makeFlags = [ "LINUX_SRCDIR=${kernelSrc}" ];

  installFlags = [ "INSTALL_MOD_PATH=$(out)" ];
}
