{ pkgs, lib
, fetchFromGitHub
, python3Packages
}:

let
  python = pkgs.python3;
in
python3Packages.buildPythonPackage rec {
  pname = "PyLibGen";
  version = "5f78604a32a97c8fc18f470231209943f93c86ed";

  src = /home/matt/src/PyLibGen;
  # src = fetchFromGitHub {
  #   owner = "NadalVRoMa";
  #   repo = pname;
  #   rev = version;
  #   sha256 = "170hyxyqm2dvsrlm25z2hcpcybqywxmsxnlqm5pdxafvahyd8257";
  # };

  propagatedBuildInputs = with python3Packages; [
    lxml
    beautifulsoup4
    tabulate
    # requests
  ];

  format = "other";

  installPhase = ''
    mkdir -p $out/bin
    cp $src/pylibgen.py $out/bin/pylibgen
    chmod +x $out/bin/pylibgen
    cp $src/settings.py $out/bin/settings.py
  '';

  meta = with lib; {
    description = "A python script to download books from libgen.io ";
    homepage = https://github.com/NadalVRoMa/PyLibGen;
    maintainers = [ maintainers.matthuszagh ];
  };
}
