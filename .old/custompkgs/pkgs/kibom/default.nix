{ fetchFromGitHub
, stdenv
, python3Packages
}:

python3Packages.buildPythonApplication rec {
  name = "kibom";
  src = fetchFromGitHub {
    owner = "SchrodingersGat";
    repo = "KiBoM";
    rev = "master";
    sha256 = "057x14sn4p6lm4rj38s13rnb7qcx23f0vrgglpiii7v6c24yqd8j";
  };

  # propagatedBuildInputs = with python3Packages; [
  #   future
  # ];

  buildInputs = with python3Packages; [
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
