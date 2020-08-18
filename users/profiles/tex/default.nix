{ pkgs
, ...
}:

{
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        collection-fontsrecommended
        scheme-small
        collection-luatex

        # latex
        collection-latex
        standalone
        pgfplots
        pgfopts
        preview

        tex4ht
        hf-tikz
        siunitx
        pythontex
        asymptote
        animate
        # dvisvgm
        # luatex
        luatex85
        pygmentex
        pdftex
        latexindent
        tikz-timing;

      # TODO can these be used directly in tpkgs?
      inherit (pkgs)
        circuitikz
        latexmk
        grffile
        dvisvgm;
    };
  };

  xdg.configFile."latexmk/latexmkrc".source = ./tex/latexmk;
  home.file."texmf/tex/latex/commonstuff/default.cls".source = ./tex/default.cls;
  home.file."texmf/tex/latex/commonstuff/tikz.cls".source = ./tex/tikz.cls;
  home.file."texmf/tex/latex/commonstuff/math.cls".source = ./tex/math.cls;
}
