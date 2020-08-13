{stdenv, fetchFromGitHub, emacs}:

stdenv.mkDerivation rec {
  name = "org-recoll";

  src = fetchFromGitHub {
    owner = "alraban";
    repo = "org-recoll";
    rev = "master";
    sha256 = "1gslf83az5qz9j3gzfvd7502r2c77vx7zvxpysw7w6xcd4f0lcc6";
  };

  buildInputs = [ emacs ];

  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp
    install *.el* $out/share/emacs/site-lisp
  '';

  meta = {
    description = "A lightweight emacs org-mode wrapper for the recoll full-text search engine";
    homepage = https://github.com/alraban/org-recoll;
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.all;
  };
}
