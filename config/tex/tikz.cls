\ProvidesClass{tikz}

% Needed for proper rendering with some corner cases in luatex
\RequirePackage{luatex85}
\PassOptionsToPackage{usenames}{xcolor}
\LoadClass[border={0pt 1pt}]{standalone}
\RequirePackage[inline]{asymptote}
\RequirePackage{booktabs}
\RequirePackage{tabularx}
\RequirePackage{bm}
\RequirePackage{siunitx}
\RequirePackage{mathtools}
\RequirePackage{tikz-timing}
\RequirePackage[american]{circuitikz}
\RequirePackage{pgfplots}
\RequirePackage{tikz}
\RequirePackage{graphicx}
\RequirePackage[normalem]{ulem}
\RequirePackage{amsmath}
\RequirePackage{textcomp}
\RequirePackage{amssymb}

%% Declared math operators
\DeclareMathOperator{\rank}{rank}

%% Circuitikz style options
\ctikzset{resistors/scale=0.6, capacitors/scale=0.6, diodes/scale=0.4}
\ctikzset{tripoles/mos style/arrows}
\ctikzset{amplifiers/scale=0.75}
\ctikzset{tripoles/op amp/font=\footnotesize}
\ctikzset{tripoles/fd op amp/font=\footnotesize}

%% tikz libraries
\usetikzlibrary{intersections}
\usetikzlibrary{3d}
\usetikzlibrary{perspective}
\usetikzlibrary{shapes.geometric}
\usetikzlibrary{decorations.markings}
\usetikzlibrary{positioning}
\usetikzlibrary{automata}
\usetikzlibrary{patterns}

%% tikz settings
\tikzset{>=latex}
% extract x and y coordinates of tikz point
\makeatletter
\newcommand{\gettikzxy}[3]{%
  \tikz@scan@one@point\pgfutil@firstofone#1\relax
  \edef#2{\the\pgf@x}%
  \edef#3{\the\pgf@y}%
}
\makeatother

%% pgfplots
\pgfplotsset{compat=newest}
\pgfplotsset{
  every non boxed x axis/.style={
    xtick align=center,
    enlarge x limits=upper,
    x axis line style={-latex},
  },
  every boxed x axis/.style={},
  every non boxed y axis/.style={
    ytick align=center,
    enlarge y limits=upper,
    y axis line style={-latex},
  },
  every boxed y axis/.style={},
}
\pgfplotsset{grid style={help lines}}
\usepgfplotslibrary{groupplots}

%% tikztiminglibraries
\usetikztiminglibrary{counters}

\endinput
