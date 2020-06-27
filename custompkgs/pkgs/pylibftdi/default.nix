{ stdenv
, python3Packages
, libftdi1
, libusb1
}:

python3Packages.buildPythonPackage rec {
  pname = "pylibftdi";
  version = "0.17.0";
  # disabled = pythonOlder "3"; # compatibility with httpserver

  src = python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "132c44rs0f2zm22hnhg93p47aaw6llpfzavc64dcqrn8719mlqfw";
  };

  propagatedBuildInputs = (with python3Packages; [
    httpserver
    libftdi1
    libusb1
  ]);

  # TODO Several modules have been renamed for python3. ReadTimeout
  # doesn't appear to be part of pylibftdi at all... These changes
  # should be upstreamed.
  patches = [
    ./make-python3-compatible.patch
    ./fix-search-paths.patch
  ];

  postPatch = ''
    substituteInPlace pylibftdi/driver.py --replace \
      "find_library('libftdi1')" \
      "'${libftdi1.out}/lib/libftdi1.so'"

    substituteInPlace pylibftdi/driver.py --replace \
      "find_library('libusb1')" \
      "'${libusb1.out}/lib/libusb1.so'"
  '';

  meta = with stdenv.lib; {
    homepage = https://bitbucket.org/codedstructure/pylibftdi/src/default/;
    description = "Minimal pythonic wrapper to Intra2net's libftdi driver for FTDI's USB devices";
    license = licenses.mit;
    maintainers = with maintainers; [ matthuszagh ];
  };
}
