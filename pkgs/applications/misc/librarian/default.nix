{ lib
, fetchFromGitHub
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  pname = "librarian";
  version = "unstable-2021-10-15";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "librarian";
    rev = "7a08cf51d5e294de63d8e3c12db2e6c3b8065dac";
    sha256 = "sha256-ht4XNVTDMgZv4Gmx5jPNFHAGIWP5iBBpadIkR5zwZi0=";
  };

  cargoSha256 = "sha256-sZjWetjYNcLxOY04S3O3bQdIXlYVDX2aSnSh35lIIVA=";

  meta = with lib; {
    description = "Digital resource management tool";
    homepage = "https://github.com/matthuszagh/librarian";
    license = licenses.unlicense;
    maintainers = [ maintainers.matthuszagh ];
  };
}
