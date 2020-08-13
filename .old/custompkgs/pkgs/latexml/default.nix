{ pkgs, stdenv, fetchurl }:

let
  perl-with-pkgs = pkgs.perl.withPackages (p: with p; [
    ArchiveZip
    DBFile
    FileWhich IOString ImageSize JSONXS LWP ParseRecDescent TextUnidecode URI XMLLibXML XMLLibXSLT
  ]);
in
stdenv.mkDerivation rec {
    pname = "LaTeXML";
    version = "0.8.4";
    src = fetchurl {
      url = "mirror://cpan/authors/id/B/BR/BRMILLER/LaTeXML-0.8.4.tar.gz";
      sha256 = "92599b45fb587ac14b2ba9cc84b85d9ddc2deaf1cbdc2e89e7a6559e1fbb34cc";
    };
    propagatedBuildInputs = [
      perl-with-pkgs
    ];

    buildPhase = ''
      perl Makefile.PL
      make
    '';

    # # tests fail
    # doCheck = false;
    meta = {
      description = "Transforms TeX and LaTeX into XML/HTML/MathML";
      license = stdenv.lib.licenses.free;
    };
}
