{ mkDerivation
, pkgs
, fetchgit
}:

mkDerivation rec {
  pname = "gnucap";
  version = "20171003";
  src = fetchgit {
    url = "https://git.savannah.gnu.org/git/gnucap.git";
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };

  # TODO needs termcap (https://www.gnu.org/software/termutils/manual/termcap-1.3/html_mono/termcap.html)
  buildInputs = with pkgs; [
    readline
  ];
}
