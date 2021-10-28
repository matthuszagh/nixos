{ lib
, fetchFromGitHub
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  pname = "librarian";
  version = "unstable-2021-10-27";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "librarian";
    rev = "83d9b171547aaee462679e4dc9f95ad1a65f8191";
    sha256 = "0i233fnvzd42na97g6jwkkni2ihp7v5v7vwmgc0d5za0f1frrlzf";
  };

  cargoSha256 = "sha256-7lctfEtMkhT7ho/tyl+5oM0yNF+RaSZSQ5gCEwxRf3o=";

  meta = with lib; {
    description = "Digital resource management tool";
    homepage = "https://github.com/matthuszagh/librarian";
    license = licenses.unlicense;
    maintainers = [ maintainers.matthuszagh ];
  };
}
