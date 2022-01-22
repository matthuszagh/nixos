\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{_standalone}

\RequirePackage{xkeyval}

%% page width
\DeclareOptionX{width}{%
  \def\width{#1}%
}

%% Initialize the page width to a column width in revtex.
\ExecuteOptionsX{width=20.5pc}

%% Process the options given to the package
\ProcessOptionsX

\RequirePackage{luatex85}
\LoadClass[varwidth=\width]{standalone}

%% Dummy widetext environment. This stops tex from issuing an error
%% when the widetext environment is used, which we need for the revtex
%% class.
\newenvironment{widetext}{}{}

\usepackage{_common}
\usepackage{_math}
\usepackage{_tikz}
