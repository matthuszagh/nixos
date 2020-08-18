{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    texlab
    perlPackages.LaTeXML
  ];
}
