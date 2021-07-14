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

        # tables
        multirow

        # math
        unicode-math
        stmaryrd

        # customizable lists
        enumitem

        # needed to build circuitikz manual
        a4wide
        titling
        titlesec
        xstring
        microtype
        showexpl
        framed
        xtab
        imakeidx
        varwidth

        tex4ht
        hf-tikz
        siunitx
        pythontex
        asymptote

        # animate and dependencies
        animate
        zref
        media9
        ocgx2

        qcircuit xypic
        luatex85
        pygmentex
        pdftex
        latexindent
        tikz-timing
        latexmk;

      inherit (pkgs)
        circuitikz
        # qcircuit-tikz
        dvisvgm
        grffile;
    };
  };

  xdg.configFile."latexmk/latexmkrc".source = ./latexmk;
  home.file."texmf/tex/latex/commonstuff/default.cls".source = ./default.cls;
  home.file."texmf/tex/latex/commonstuff/tikz.cls".source = ./tikz.cls;
  home.file."texmf/tex/latex/commonstuff/math.cls".source = ./math.cls;
  home.file."texmf/tex/latex/commonstuff/common_local.sty".source = ./common_local.sty;
  home.file."texmf/tex/latex/commonstuff/math_local.sty".source = ./math_local.sty;
  home.file."texmf/tex/latex/commonstuff/math_op_local.tex".source = ./math_op_local.tex;
  home.file."texmf/tex/latex/commonstuff/tikz_local.sty".source = ./tikz_local.sty;
}
