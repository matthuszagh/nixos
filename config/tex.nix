{ pkgs, ... }:

let custompkgs = import <custompkgs> { };
in
{
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        collection-fontsrecommended
        scheme-small
        collection-latexextra
        collection-luatex

        tex4ht
        hf-tikz
        siunitx
        pythontex
        asymptote
        # dvisvgm
        # luatex
        luatex85
        pygmentex
        pdftex
        latexindent
        tikz-timing;

      inherit (custompkgs)
        circuitikz
        dvisvgm
        grffile
        latexmk;
    };
  };

  xdg.configFile."latexmk/latexmkrc".source = ./tex/latexmk;
  home.file."texmf/tex/latex/commonstuff/default.cls".source = ./tex/default.cls;
  home.file."texmf/tex/latex/commonstuff/tikz.cls".source = ./tex/tikz.cls;
  home.file."texmf/tex/latex/commonstuff/math.cls".source = ./tex/math.cls;
}
