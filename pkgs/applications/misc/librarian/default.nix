{ lib
, fetchFromGitHub
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  pname = "librarian";
  version = "unstable-2021-11-22";

  src = fetchFromGitHub {
    owner = "matthuszagh";
    repo = "librarian";
    rev = "e9c865140724f63ca05af9722183a33432cdb6c2";
    sha256 = "sha256-UvHcYnKrPkMHWzXe85Qia55zAUqGLGub5bgfgdxkBEg=";
  };

  cargoSha256 = "sha256-3oPuIIqH0plA6vDZOpka3ETreEwH6sneQU16XSXM/Ho=";

  meta = with lib; {
    description = "Digital resource management tool";
    homepage = "https://github.com/matthuszagh/librarian";
    license = licenses.unlicense;
    maintainers = [ maintainers.matthuszagh ];
  };
}

