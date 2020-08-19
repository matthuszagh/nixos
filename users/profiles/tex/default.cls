\ProvidesClass{default}

\LoadClass[10pt,twoside,a4paper]{report}

\RequirePackage{fullpage}
\RequirePackage{hyperref}
\RequirePackage{mathtools}
\RequirePackage{booktabs,caption,subcaption}
\captionsetup[table]{aboveskip=0pt,font=bf}
\captionsetup[figure]{aboveskip=0pt,font=bf}
\captionsetup[subfigure]{font=small, skip=0pt, labelformat=empty}
\RequirePackage{ltablex}
\RequirePackage{makeidx}
\RequirePackage{amssymb}
\RequirePackage[dvipdfm]{graphicx}
\RequirePackage{bmpsize}
\RequirePackage{tabularx}
\RequirePackage{gensymb}
\RequirePackage{pdfpages}
\RequirePackage{amsmath}
\RequirePackage{bm}
\RequirePackage[nomargin,inline,marginclue,draft]{fixme}
\RequirePackage{listings}
\RequirePackage{color}
\RequirePackage{float}
\RequirePackage[makeroom]{cancel}
\RequirePackage[group-separator={,}]{siunitx}
\RequirePackage{parskip}
\RequirePackage{cleveref}
\RequirePackage{tabu}
\RequirePackage{xcolor}
\RequirePackage{minted}
\RequirePackage{tikz}
\RequirePackage[siunitx, american]{circuitikz}
\RequirePackage{pythontex}
\RequirePackage{titlesec}
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}
\RequirePackage[toc,page]{appendix}
\RequirePackage{enumitem}
\RequirePackage{incgraph}

\crefname{section}{\S}{\S\S}
\Crefname{section}{\S}{\S\S}

\DeclareMathOperator{\csch}{csch}
\DeclareMathOperator{\sinc}{sinc}
\setlength\parindent{0pt}
\setlength\fboxrule{.3mm}
\DeclarePairedDelimiter\ceil{\lceil}{\rceil}
\DeclarePairedDelimiter\floor{\lfloor}{\rfloor}

\definecolor{bg-dark}{HTML}{222222}
\definecolor{bg-gray}{HTML}{EFF0F1}

\hypersetup{
    colorlinks=true,
    linkcolor=black,
    urlcolor=blue,
}

% tabularx configuration.
\newcolumntype{C}{>{\centering\arraybackslash}X}
\newcommand\mc[1]{\multicolumn{1}{C}{#1}}

% Creates \textoverline{} to provide an overline outside math mode.
\makeatletter
\newcommand*{\textoverline}[1]{$\overline{\hbox{#1}}\m@th$}
\makeatother

% Minted configuration
\setminted{mathescape,breaklines,bgcolor=bg-dark,linenos,numbersep=5pt,escapeinside=||}
\setminted[text]{mathescape,breaklines,bgcolor=bg-gray,linenos=0,escapeinside=||,frame=lines,framesep=2mm}
\usemintedstyle{sourcerer}

% Header spacing
\titlespacing\section{0pt}{5ex plus 1ex minus 0.2ex}{2ex plus 0.5ex minus 0.1ex}
\titlespacing\subsection{0pt}{5ex plus 1ex minus 0.2ex}{2ex plus 0.5ex minus 0.1ex}
\titlespacing\subsubsection{0pt}{5ex plus 1ex minus 0.2ex}{2ex plus 0.5ex minus 0.1ex}

\def\dbar{{\mathchar'26\mkern-12mu d}}

\makeindex

\endinput
