{ lib
, fetchFromGitHub
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  pname = "librarian";
  version = "unstable-2021-10-20";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "librarian";
    rev = "395e4a705affbc7a44b2ffbae6b4674af76ed605";
    sha256 = "1bw6vnw41swq6s7d8aira305bgi3hzifmvs4zfzcj0p5hhhas8m2";
  };

  cargoSha256 = "sha256-0VBryXbT54DhjrMDbs3/l/9Z4i7xHnrLYPLbTvnncCc=";

  meta = with lib; {
    description = "Digital resource management tool";
    homepage = "https://github.com/matthuszagh/librarian";
    license = licenses.unlicense;
    maintainers = [ maintainers.matthuszagh ];
  };
}
