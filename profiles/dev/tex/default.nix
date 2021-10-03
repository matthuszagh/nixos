{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    texlab
    texlive.combined.scheme-full
  ];
}
