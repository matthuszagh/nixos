{ pkgs
, ...
}:

{
  environment.systemPackages = with pkgs; [
    k2pdfopt
    anystyle-cli
    poppler_utils
    koreader
    pdftk
  ];
}
