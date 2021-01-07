{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    texlab
    # TODO fix latexml
    LaTeXML
  ];
}
