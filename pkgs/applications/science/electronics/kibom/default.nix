{ fetchFromGitHub
, stdenv
, buildPythonApplication
, XlsxWriter
}:

buildPythonApplication rec {
  pname = "kibom";
  version = "unstable-2020-08-15";

  src = fetchFromGitHub {
    owner = "SchrodingersGat";
    repo = "KiBoM";
    rev = "TODO";
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };

  buildInputs = [
    XlsxWriter
  ];

  # format = "other";

  # installPhase = ''
  #   mkdir -p $out/bin
  #   cp $src/KiBOM_CLI.py $out/bin/kibom
  #   chmod +x $out/bin/kibom
  #   cp -r $src/bomlib $out/bin/bomlib
  # '';
}
