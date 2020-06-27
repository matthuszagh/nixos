{ stdenv, pkgs }:

stdenv.mkDerivation rec {
  pname = "gnucap";
  version = "20171003";
  src = /home/matt/src/gnucap;

  # TODO needs termcap (https://www.gnu.org/software/termutils/manual/termcap-1.3/html_mono/termcap.html)
  buildInputs = with pkgs; [
    readline
  ];
}
