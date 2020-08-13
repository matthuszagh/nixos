{ stdenv, pkgs }:

stdenv.mkDerivation rec{
  name = "tree-sitter-wrapped";
  src = pkgs.tree-sitter.src;

  buildInputs = with pkgs; [
    tree-sitter
    graphviz
  ];
}
