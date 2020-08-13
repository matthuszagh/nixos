{ stdenv, fetchFromGitHub
, notmuch
, python3Packages
}:

stdenv.mkDerivation rec {
  pname = "lieer";
  version = "v1.0";

  src = fetchFromGitHub {
    owner = "gauteh";
    repo = "lieer";
    rev = version;
    sha256 = "0000000000000000000000000000000000000000000000000000";
  };

  buildInputs = [
    notmuch
  ] ++ (with python3Packages; [
    google_api_python_client
    oauth2client
    tqdm
  ]);

  # meta = with stdenv.lib; {
  #   description = "A refreshed Python toolbox for building complex digital hardware";
  #   homepage    = "https://lambdaconcept.com";
  #   license     = licenses.bsd2;
  #   maintainers = [ maintainers.sb0 ];
  # };
}
