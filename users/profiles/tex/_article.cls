\ProvidesClass{_article}

% \RequirePackage{luatex85}
% \PassOptionsToPackage{usenames}{xcolor}
% \LoadClass[10pt, letterpaper]{article}

% \RequirePackage{math_local}
% \RequirePackage{tikz_local}
% \RequirePackage{common_local}
% \RequirePackage{svg}
% \RequirePackage{incgraph}
% \RequirePackage{hyperref}
% \RequirePackage{cleveref}
% \RequirePackage{minted}

% \setminted{fontsize=\footnotesize}
% \setminted{linenos=true}

% \textwidth=30pc


\RequirePackage{luatex85}
\LoadClass[twocolumn, secnumarabic, amssymb, nobibnotes, aps, prd]{revtex4-2}

\usepackage{math_local}
\usepackage{tikz_local}
\usepackage{common_local}

\usepackage{svg}
\usepackage{incgraph}
\usepackage{hyperref}
\usepackage{cleveref}
\usepackage{minted}
\usepackage{babel}
\usepackage[style=american]{csquotes}
\usepackage{booktabs}
\usepackage{tcolorbox}
\usepackage{fancyvrb}
\usepackage{caption}

% csquotes
\MakeOuterQuote{"}

\setminted{fontsize=\footnotesize}
\setminted{linenos=true}
\setminted{numbersep=4pt}
% place lines above and below code blocks
%%\setminted{frame=lines}
% break up long lines
\setminted{breaklines=true}
\definecolor{codebg}{rgb}{0.95,0.95,0.95}
% \setminted{bgcolor=codebg}

\tcbuselibrary{breakable}
\tcbset{colback=codebg, colframe=codebg, left=0mm, right=0mm, top=0mm,
  bottom=0mm, leftrule=0mm, rightrule=0mm, toprule=0mm,
  bottomrule=0mm, breakable=true}

% wrap all minted environments in tcolorbox
% TODO we should probably remove this when we determine a better way
\BeforeBeginEnvironment{minted}{\begin{tcolorbox}}
\AfterEndEnvironment{minted}{\end{tcolorbox}}

\newenvironment{code}{\captionsetup{type=listing}}{}

\hypersetup{
  colorlinks=false,
  pdfborder={0 0 0}
  % colorlinks=true,
  % allcolors=blue
}
