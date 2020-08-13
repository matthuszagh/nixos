{ fetchFromGitHub, stdenv, python3Packages }:

python3Packages.buildPythonApplication rec {
  name = "brainworkshop";
  src = fetchFromGitHub {
    owner = "samcv";
    repo = "brainworkshop";
    rev = "a4b52df6839d345654a114f2cf3e216d214c68f5";
    sha256 = "0b0xgz3q130v8k3rkwi3bldcagfn4zzff543slxxllpz83aibk30";
  };

  propagatedBuildInputs = with python3Packages; [
    pyglet
    future
  ];

  format = "other";

  installPhase = ''
    mkdir -p $out/bin
    cp $src/brainworkshop.pyw $out/bin/brainworkshop
    chmod +x $out/bin/brainworkshop
    cp -r $src/data $out/bin/data
    cp -r $src/res $out/bin/res
    cp -r $src/tools $out/bin/tools
  '';
}
