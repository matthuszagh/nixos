{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    texlab
    # TODO fix latexml
    LaTeXML
    texlive.combined.scheme-full
  ];
}
