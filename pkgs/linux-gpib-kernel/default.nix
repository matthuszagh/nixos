{ stdenv
, fetchurl
# , fetchzip
, fetchsvn
, kernel
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

  # src = fetchzip {
  #   url = "https://sourceforge.net/code-snapshots/svn/l/li/linux-gpib/code/linux-gpib-code-r1996-branches-v4_4_0.zip";
  #   sha256 = "sha256-NcPrXMLYoCLOSoQdJFVW8fczcvOR+SwMZLhUtG7UPxA=";
  # };

  # src = fetchsvn {
  #   url = "https://svn.code.sf.net/p/linux-gpib/code/trunk";
  #   sha256 = "sha256-tZmbxxQGhqu2hYLrHFqMzUJlrA81D3fhUsW1A1iGJ2Q=";
  #   rev = "1984";
  # };

  buildInputs = kernel.moduleBuildDependencies;

  kernelVersion = kernel.dev.modDirVersion;
  kernelSrc = "${kernel.dev}/lib/modules/${kernelVersion}/build";

  unpackPhase = ''
    tar xf $src
    tar xf ${pname}-${version}/${pname}-kernel-${version}.tar.gz
  '';

  # postUnpack = ''
  #   rm -r $sourceRoot/linux-gpib-user-${version}
  #   mv $sourceRoot/linux-gpib-kernel-${version}/* $sourceRoot/.
  #   rm -r $sourceRoot/linux-gpib-kernel-${version}
  # '';

  # patches = [
  #   ./linux-gpib-kernel-4.3.4.patch
  # ];

  postPatch = ''
    sed -i 's@/sbin/depmod -A@@g' Makefile
  '';

  sourceRoot = "${pname}-kernel-${version}";

  buildFlags = [
    "ENABLE_DEVICE=1"
    "ENABLE_TNT5004=1"
  ];

  makeFlags = [
    "LINUX_SRCDIR=${kernelSrc}"
  ];

  installFlags = [ "INSTALL_MOD_PATH=$(out)" ];
}
