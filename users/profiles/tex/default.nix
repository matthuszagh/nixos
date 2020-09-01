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
        quantikz xargs xstring environ trimspaces tikz-cd
        luatex85
        pygmentex
        pdftex
        latexindent
        tikz-timing;

      inherit (pkgs)
        circuitikz
        dvisvgm
        grffile
        latexmk;
    };
  };

  xdg.configFile."latexmk/latexmkrc".source = ./latexmk;
  home.file."texmf/tex/latex/commonstuff/default.cls".source = ./default.cls;
  home.file."texmf/tex/latex/commonstuff/tikz.cls".source = ./tikz.cls;
  home.file."texmf/tex/latex/commonstuff/math.cls".source = ./math.cls;
  home.file."texmf/tex/latex/commonstuff/common_local.sty".source = ./common_local.sty;
  home.file."texmf/tex/latex/commonstuff/math_local.sty".source = ./math_local.sty;
  home.file."texmf/tex/latex/commonstuff/tikz_local.sty".source = ./tikz_local.sty;
}
