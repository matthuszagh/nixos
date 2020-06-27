{ stdenv, fetchFromGitHub, python3Packages, nmigen }:

python3Packages.buildPythonPackage {
  name = "nmigen-boards";

  src = fetchFromGitHub {
    owner = "m-labs";
    repo = "nmigen-boards";
    rev = "bb52dfb9575c4aabf8e7dd9ddd780bf342ee0a71";
    sha256 = "18qcydakghfpz500n792mymad9bh1550xrl6rcbqwwbvcc6yss46";
  };

  propagatedBuildInputs = [ nmigen ];

  meta = with stdenv.lib; {
    description = "Board and connector definition files for nMigen";
    homepage    = "https://m-labs.hk";
    license     = licenses.bsd2;
    maintainers = [ maintainers.sb0 ];
  };
}
