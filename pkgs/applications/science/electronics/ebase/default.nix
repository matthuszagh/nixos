{ fetchFromGitHub
, stdenv
, buildPythonApplication
, kibom
, pyyaml
, psycopg2
, pyusb
}:

buildPythonApplication rec {
  pname = "ebase";
  version = "unstable-2019-02-27";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "ebase";
    rev = "TODO";
    sha256 = "TODO";
  };

  propagatedBuildInputs = [
    pyyaml
    psycopg2
    pyusb
    kibom
  ];

  format = "other";

  installPhase = ''
    mkdir -p $out/bin
    cp $src/ebase.py $out/bin/ebase
    chmod +x $out/bin/ebase
    cp -r $src/ebase_config $out/bin/ebase_config
  '';
}
