{ pkgs
, ...
}:

{
  xdg.configFile."latexmk/latexmkrc".source = ./latexmk;
  home.file."texmf/tex/latex/commonstuff/default.cls".source = ./default.cls;
  home.file."texmf/tex/latex/commonstuff/tikz.cls".source = ./tikz.cls;
  home.file."texmf/tex/latex/commonstuff/math.cls".source = ./math.cls;
  home.file."texmf/tex/latex/commonstuff/common_local.sty".source = ./common_local.sty;
  home.file."texmf/tex/latex/commonstuff/math_local.sty".source = ./math_local.sty;
  home.file."texmf/tex/latex/commonstuff/math_op_local.tex".source = ./math_op_local.tex;
  home.file."texmf/tex/latex/commonstuff/tikz_local.sty".source = ./tikz_local.sty;
}
